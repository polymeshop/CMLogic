{-# LANGUAGE MultiParamTypeClasses, NamedFieldPuns #-} 
module MLogic.HighLevel.Codegen where

import MLogic.HighLevel.Types

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except

import Data.Endo

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

import MLogic.Constants
import MLogic.Assembly.Op
import MLogic.Assembly.Operand
import MLogic.Assembly.GenAsm.Types
import qualified MLogic.Assembly.GenAsm.Utils as G
import MLogic.Assembly.GenAsm.Utils (IsInstr)
import MLogic.HighLevel.TC (getTCedExprTy)
import Data.List

import System.IO

import Text.Parsec.Pos

noLoc = initialPos "(no location)"

-- the codegen state should (mostly) be used across functions, you don't want the allocated ins vars to overlap (which might be used for the parameters of different params)
-- input vars should not be mangled

data VarAlloc = AllocVar String | AllocTuple [VarAlloc] | AllocIgnore deriving Show

data ExtInstr = ExtInstr Instr
              | ExtFunCallIns String  String
               --             funName retAddr
              | ExtFunRet Operand -- return instruction with return address

instance Show ExtInstr where
  show (ExtInstr ins) = show ins
  show (ExtFunCallIns funName retAddr) = "[pseudo op] call(" <> funName <> ", " <> "ret: " <> show retAddr <> ")"
  show (ExtFunRet retAddr) = "[pseudo op] return(" <> show retAddr <> ")"
instance IsInstr ExtInstr where
  fromInstr = ExtInstr
  toInstr (ExtInstr i) = Just i
  toInstr _ = Nothing

data CodegenState = CodegenState { codegenInsVarSet :: Set String
                                 , codegenVarMap :: Map String VarAlloc
                                    -- a map from variable name in the source to one or more variable names in the alllocated ins var
                                 , codegenVarCounter :: Map String Integer
                                 , codegenLabelCounter :: Integer
                                 , codegenScopeLevel :: Integer
                                 , codegenBuiltinFuncMap :: Map String BuiltinFunction
                                 , codegenFunParamRetAlloc :: Map String ([(String, VarAlloc)], VarAlloc, String)
                                   -- allocation result for function parameter & return result & return addr alloc (in this sequence)
                                 , codegenFuncSigMap :: Map String ([(String, SourcePos, Types)], Types)
                                 , codegenFuncEntryMap :: Map String String -- function entry point label map
                                 , codegenLogger :: String -> IO ()
                                 , codegenFunName :: String
                                 , codegenIns :: Endo [ExtInstr]
                                 }

initCodegenState = CodegenState { codegenInsVarSet = S.empty
                                , codegenVarMap = M.empty
                                , codegenVarCounter = M.empty
                                , codegenLabelCounter = 0
                                , codegenScopeLevel = 0
                                , codegenBuiltinFuncMap = M.empty
                                , codegenFunParamRetAlloc = M.empty
                                , codegenFuncSigMap = M.empty
                                , codegenFuncEntryMap = M.empty
                                , codegenLogger = hPutStrLn stderr
                                , codegenFunName = ""
                                , codegenIns = emptyCodegenIns
                                }

runCodegen :: Codegen a -> IO (Either String (a, CodegenState))
runCodegen cg = runExceptT $ runStateT cg initCodegenState

type Codegen = StateT CodegenState (ExceptT String IO)

emptyCodegenIns = Endo id

liftGenAsm :: GenAsmM ExtInstr Identity a -> Codegen a
liftGenAsm p = do
  CodegenState{codegenInsVarSet, codegenLabelCounter, codegenVarCounter} <- get
  let initGenAsmState =
          GenAsmState { genAsmVarCounter = codegenVarCounter
                      , genAsmLabVar = codegenLabelCounter
                      , genAsmInstrs = Endo id
                      , genAsmVars = codegenInsVarSet }
                      
      r = runIdentity $ runGenAsmM p initGenAsmState
  case r of
    Left error -> throwError error
    Right (genAsmState', result) -> do
      modify (\s -> s { codegenVarCounter = genAsmVarCounter genAsmState'
                      , codegenLabelCounter = genAsmLabVar genAsmState'
                      , codegenInsVarSet = genAsmVars genAsmState'
                      , codegenIns = composeEndo (codegenIns s) (genAsmInstrs genAsmState')
                      })
      return result

ga :: GenAsmM ExtInstr Identity a -> Codegen a
ga = liftGenAsm 

addExtInstr :: ExtInstr -> Codegen ()
addExtInstr i = do
  ins <- gets codegenIns
  modify $ \s -> s { codegenIns = composeEndo ins (Endo ([i]++)) }


addInstr :: Instr -> Codegen ()
addInstr i = ga (G.addInstr i)

newLab :: Codegen String
newLab = ga G.newLab

addLab :: String -> Codegen ()
addLab = ga . G.addLab 

addNewLab :: Codegen String
addNewLab = ga G.addNewLab

codegenError :: String -> Codegen a
codegenError s = throwError $ "internal codegen error: " <> s

-- allocate insVars at params / decls

newScope :: Codegen a -> Codegen a
newScope p = do
  currentScope <- gets codegenScopeLevel
  currentVarMap <- gets codegenVarMap
  modify (\s -> s { codegenScopeLevel = currentScope + 1 })
  r <- p
  modify (\s -> s { codegenScopeLevel = currentScope, codegenVarMap = currentVarMap })
  return r
clearIns :: Codegen ()
clearIns = modify (\s -> s { codegenIns = emptyCodegenIns })

newVarAlloc :: Codegen VarAlloc
newVarAlloc = fmap AllocVar $ ga G.newVar

newVarAllocWithHint :: String -> Codegen VarAlloc
newVarAllocWithHint = fmap AllocVar . ga . G.newVarWithHint

allocVars :: Types -> Codegen VarAlloc
allocVars VarTy = newVarAlloc
allocVars (TupleTy tys) = do
  allocedNames <- mapM allocVars tys
  return (AllocTuple allocedNames)


allocVarsWithHint :: String -> Types -> Codegen VarAlloc
allocVarsWithHint name VarTy = newVarAllocWithHint name
allocVarsWithHint name (TupleTy tys) = do
  allocedNames <- mapM (allocVarsWithHint name) tys
  return (AllocTuple allocedNames)

allocVarsInCurScope :: String -> Types -> Codegen VarAlloc
allocVarsInCurScope name ty = do
    vars <- allocVarsWithHint name ty
    varMap <- gets codegenVarMap
    modify (\s -> s { codegenVarMap = M.insert name vars varMap })
    return vars


loadAllocedVarInCurScope :: String -> VarAlloc -> Codegen ()
loadAllocedVarInCurScope name alloc = do
  varMap <- gets codegenVarMap
  modify $ \s -> s { codegenVarMap = M.insert name alloc varMap }

allocPatsInCurScopes :: Pat SourcePos -> Types -> Codegen VarAlloc
allocPatsInCurScopes (VarPat _ str) ty = allocVarsInCurScope str ty
allocPatsInCurScopes (IgnorePat _) _ = return $ AllocIgnore
allocPatsInCurScopes (TuplePat _ pats) (TupleTy tys) = do
  r <- sequence $ zipWith allocPatsInCurScopes pats tys
  return $ AllocTuple r
allocPatsInCurScopes pat ty = codegenError $ "allocPatsInCurScopes: " <> "incompatible pat " <> show pat <> " and type " <> show ty <> " combination"

lookupPatInsVars :: Pat SourcePos -> Codegen VarAlloc
lookupPatInsVars (VarPat _ str) = do
  varMap <- gets codegenVarMap
  case M.lookup str varMap of
    Just alloc -> return alloc
    Nothing -> codegenError $ "lookupPatInsVars: failed to find variable " <> str <> " in codegenVarMap"
lookupPatInsVars (IgnorePat _) = return AllocIgnore
lookupPatInsVars (TuplePat _ pats) = do
  allocs <- mapM lookupPatInsVars pats
  return $ AllocTuple allocs

getFunCallArgDepth :: FunCallArg loc -> Integer
getFunCallArgDepth (ExprArg _ expr) = getExprDepth expr
getFunCallArgDepth (TargetArg _ _) = 0
getFunCallArgDepth (SortArg _ _) = 0
getFunCallArgDepth (BuildingGroupArg _ _) = 0

getExprDepth :: Expr loc -> Integer
getExprDepth (FunCallExpr _ _ funName callArgs) = foldl' max 0 (map getFunCallArgDepth callArgs) + 1
getExprDepth (Add _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (Sub _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (Mul _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (Div _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (Mod _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (Pow _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (Equal _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (NotEqual _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (LAnd _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (LessThan _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (LessThanEq _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (GreaterThan _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (GreaterThanEq _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (StrictEqual _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (StrictNEq _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (Shl _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (Shr _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (LOr _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (BOr _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (BAnd _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (Xor _ _ expr1 expr2) = max (getExprDepth expr1) (getExprDepth expr2) + 1
getExprDepth (Flip _ _ expr1) = getExprDepth expr1 + 1
getExprDepth (Not _ _ expr1) = getExprDepth expr1 + 1
getExprDepth (Negate _ _ expr1) = getExprDepth expr1 + 1
getExprDepth (Tuple _ _ exprs) = foldl' max 0 (map getExprDepth exprs)
getExprDepth (Lit _ _ _) = 0
getExprDepth (Var _ _ _) = 0


exprIsBuiltinOp :: Expr loc -> Codegen Bool
exprIsBuiltinOp (FunCallExpr _ _ funName _) = do
  builtinFuncMap <- gets codegenBuiltinFuncMap
  case M.lookup funName builtinFuncMap of
    Just _ -> return True
    Nothing -> return False
exprIsBuiltinOp (Add _ _ expr1 expr2) = return True
exprIsBuiltinOp (Sub _ _ expr1 expr2) = return True
exprIsBuiltinOp (Mul _ _ expr1 expr2) = return True
exprIsBuiltinOp (Div _ _ expr1 expr2) = return True
exprIsBuiltinOp (Mod _ _ expr1 expr2) = return True
exprIsBuiltinOp (Pow _ _ expr1 expr2) = return True
exprIsBuiltinOp (Equal _ _ expr1 expr2) = return True
exprIsBuiltinOp (NotEqual _ _ expr1 expr2) = return True
exprIsBuiltinOp (LAnd _ _ expr1 expr2) = return True
exprIsBuiltinOp (LessThan _ _ expr1 expr2) = return True
exprIsBuiltinOp (LessThanEq _ _ expr1 expr2) = return True
exprIsBuiltinOp (GreaterThan _ _ expr1 expr2) = return True
exprIsBuiltinOp (GreaterThanEq _ _ expr1 expr2) = return True
exprIsBuiltinOp (StrictEqual _ _ expr1 expr2) = return True
exprIsBuiltinOp (StrictNEq _ _ expr1 expr2) = return True
exprIsBuiltinOp (Shl _ _ expr1 expr2) = return True
exprIsBuiltinOp (Shr _ _ expr1 expr2) = return True
exprIsBuiltinOp (LOr _ _ expr1 expr2) = return True
exprIsBuiltinOp (BOr _ _ expr1 expr2) = return True
exprIsBuiltinOp (BAnd _ _ expr1 expr2) = return True
exprIsBuiltinOp (Xor _ _ expr1 expr2) = return True
exprIsBuiltinOp (Flip _ _ expr1) = return True
exprIsBuiltinOp (Not _ _ expr1) = return True
exprIsBuiltinOp (Negate _ _ expr1) = return True
exprIsBuiltinOp (Tuple _ _ exprs) = return False
exprIsBuiltinOp (Lit _ _ _) = return False
exprIsBuiltinOp (Var _ _ _) = return False


-- varAlloc is where the result of the expression should be stored
-- varAlloc should only be used when jumpTarget == Nothing
-- if jumpTarget is not Nothing,
-- jump to jumpTarget when the expression is satisfied
-- varAlloc is not used when jumpTarget is not Nothing

varTyLitToOperand :: CL -> Codegen Operand
varTyLitToOperand (AtConstant s) = return $ G.var s
varTyLitToOperand (DoubleLit d) = return $ G.double d
varTyLitToOperand (StrLit s) = return $ G.str s
varTyLitToOperand TrueL = return trueC
varTyLitToOperand FalseL = return falseC
varTyLitToOperand Null = return nullC
varTyLitToOperand cl = codegenError $ "VarTyLitToOperand: not a VarTyLit: " <> show cl

assignLitToAlloc :: VarAlloc -> CL -> Codegen ()
assignLitToAlloc (AllocVar str) cl = do
  clOperand <- varTyLitToOperand cl
  addInstr $ SetI (G.var str) clOperand
assignLitToAlloc (AllocTuple allocs) (TupleLit lits) = sequence_ $ zipWith assignLitToAlloc allocs lits
assignLitToAlloc AllocIgnore _ = return ()
assignLitToAlloc alloc cl = codegenError $ "assignLitToAlloc: unexpected alloc " <> show alloc <> " and cl " <> show cl <> " combination"


-- non ignore VarAllocs can be turned into expr
-- should not conflict with variables in the original program
allocToExpr :: VarAlloc -> Types -> Codegen (Expr SourcePos)
allocToExpr (AllocVar alloc) ty = do
  modify $ \s -> s { codegenVarMap = M.insert alloc (AllocVar alloc) (codegenVarMap s) }
  return $ Var noLoc (Just ty) alloc
allocToExpr (AllocTuple allocs) ty@(TupleTy tys) = do
  exprs <- sequence $ zipWith allocToExpr allocs tys
  return $ Tuple noLoc (Just ty) exprs
allocToExpr _ _ = codegenError "allocToExpr"
-- compile expr into depth 0 exprs
codegenExpr' :: Expr SourcePos -> Codegen (Expr SourcePos)
codegenExpr' e@(Var _ (Just ty) v) = do
  alloc <- lookupPatInsVars (VarPat noLoc v)
  allocToExpr alloc ty
codegenExpr' e@(Lit _ _ _) = return e
codegenExpr' e@(Tuple _ ty exprs) = do
  exprs' <- mapM codegenExpr' exprs
  return $ (Tuple noLoc ty exprs')
codegenExpr' e = do
  v1 <- ga G.newVar -- starts with underscore and would not conflict with variable names in the original program
                    -- important!
  
  modify $ \s -> s { codegenVarMap = M.insert v1 (AllocVar v1) (codegenVarMap s) }
  codegenExpr e (AllocVar v1) Nothing
  return (Var noLoc (Just VarTy) v1)
-- compiler sub expressions to depth 1, and call codegenExpr
codegenBuiltinNonDepth1CompExpr :: Expr SourcePos -> VarAlloc -> Maybe String -> Codegen ()
codegenBuiltinNonDepth1CompExpr (FunCallExpr _ ty funName funCallArgs) alloc jumpTarget = do
  newFunCallArgs <- flip mapM funCallArgs $ \funCallArg -> do
                      case funCallArg of
                        ExprArg _ expr -> do
                          e1 <- codegenExpr' expr
                          return $ ExprArg noLoc e1
                        _ -> return funCallArg
  codegenExpr (FunCallExpr noLoc ty funName newFunCallArgs) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (Add _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (Add noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (Sub _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (Add noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (Mul _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (Mul noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (Div _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (Div noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (Mod _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (Mod noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (Pow _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (Pow noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (Equal _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (Equal noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (NotEqual _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (NotEqual noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (LAnd _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (LAnd noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (LessThan _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (LessThan noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (LessThanEq _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (LessThanEq noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (GreaterThan _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (GreaterThan noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (GreaterThanEq _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (GreaterThanEq noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (StrictEqual _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (StrictEqual noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (StrictNEq _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (StrictNEq noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (Shl _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (Shl noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (Shr _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (Shr noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (LOr _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (LOr noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (BOr _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (BOr noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (BAnd _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (BAnd noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (Xor _ _ expr1 expr2) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  e2 <- codegenExpr' expr2
  codegenExpr (Xor noLoc (Just VarTy) e1 e2) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (Flip _ _ expr1) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  codegenExpr (Flip noLoc (Just VarTy) e1) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (Not _ _ expr1) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  codegenExpr (Not noLoc (Just VarTy) e1) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr (Negate _ _ expr1) alloc jumpTarget = do
  e1 <- codegenExpr' expr1
  codegenExpr (Negate noLoc (Just VarTy) e1) alloc jumpTarget
codegenBuiltinNonDepth1CompExpr e _ _ = codegenError $ "unhandled in codegenBuiltinNonDepth1CompExpr: " <> show e




varTyExprLeafOperand :: Show loc => Expr loc -> Codegen Operand
varTyExprLeafOperand (Lit _ _ cl) = varTyLitToOperand cl
varTyExprLeafOperand (Var _ _ v) = do
  varMap <- gets codegenVarMap
  case M.lookup v varMap of
    Just alloc -> case alloc of
                    AllocVar alloc -> return (G.var alloc)
                    _ -> codegenError $ "impossible in varTyExprLeafOperand"
    Nothing -> codegenError $ "varTyExprLeafOperand: variable " <> v <> " not found in varMap"
varTyExprLeafOperand e = codegenError $ "varTyExprLeafOperand: expr " <> show e <> " is not a varTyExprLeaf" 
  
varTyVarAllocGetVar :: VarAlloc -> Codegen String
varTyVarAllocGetVar (AllocVar v) = return v
varTyVarAllocGetVar AllocIgnore = return "null"
varTyVarAllocGetVar alloc = codegenError $ "varTyVarAllocGetVar: unhandled alloc: " <> show alloc 



-- assignAllocToAlloc lhsAlloc rhsAlloc
assignAllocToAlloc :: VarAlloc -> VarAlloc -> Codegen ()
assignAllocToAlloc (AllocVar s) (AllocVar s') = addInstr $ SetI (G.var s) (G.var s')
assignAllocToAlloc (AllocTuple allocs) (AllocTuple allocs') = sequence_ $ zipWith assignAllocToAlloc allocs allocs'
assignAllocToAlloc AllocIgnore _ = return ()
assignAllocToAlloc lhsAlloc rhsAlloc = codegenError $ "assignAllocToAlloc: failed to assign rhs " <> show rhsAlloc <> " to lhs " <> show lhsAlloc


getDepth1TupleAllocVars :: VarAlloc -> Codegen [String]
getDepth1TupleAllocVars (AllocTuple allocs) = mapM varTyVarAllocGetVar allocs
getDepth1TupleAllocVars alloc = codegenError $ "getDepth1TupleAllocVars: non depth1 alloc: " <> show alloc

codegenExpr :: Expr SourcePos -> VarAlloc -> Maybe String -> Codegen ()
codegenExpr (Not _ ty (StrictEqual _ _ expr1 expr2)) resultAlloc jumpTarget = codegenExpr (StrictNEq noLoc ty expr1 expr2) resultAlloc jumpTarget
codegenExpr (Not _ ty (StrictNEq _ _ expr1 expr2)) resultAlloc jumpTarget = codegenExpr (StrictEqual noLoc ty expr1 expr2) resultAlloc jumpTarget
codegenExpr (Not _ ty (Equal _ _ expr1 expr2)) resultAlloc jumpTarget = codegenExpr (NotEqual noLoc ty expr1 expr2) resultAlloc jumpTarget
codegenExpr (Not _ ty (NotEqual _ _ expr1 expr2)) resultAlloc jumpTarget = codegenExpr (Equal noLoc ty expr1 expr2) resultAlloc jumpTarget
codegenExpr (Not _ ty (LessThan _ _ expr1 expr2)) resultAlloc jumpTarget = codegenExpr (GreaterThanEq noLoc ty expr1 expr2) resultAlloc jumpTarget
codegenExpr (Not _ ty (LessThanEq _ _ expr1 expr2)) resultAlloc jumpTarget = codegenExpr (GreaterThan noLoc ty expr1 expr2) resultAlloc jumpTarget
codegenExpr (Not _ ty (GreaterThan _ _ expr1 expr2)) resultAlloc jumpTarget = codegenExpr (LessThanEq noLoc ty expr1 expr2) resultAlloc jumpTarget
codegenExpr (Not _ ty (GreaterThanEq _ _ expr1 expr2)) resultAlloc jumpTarget = codegenExpr (LessThan noLoc ty expr1 expr2) resultAlloc jumpTarget
codegenExpr (Not _ _ (Not _ _ expr)) resultAlloc jumpTarget = codegenExpr expr resultAlloc jumpTarget
-- handle the base cases first
codegenExpr (Lit _ _ cl) resultAlloc jumpTarget =
  case jumpTarget of
    Just jumpTarget -> do
      operand <- varTyLitToOperand cl
      ga $ G.jumpLabelSEq jumpTarget operand trueC
    Nothing -> do
      assignLitToAlloc resultAlloc cl
codegenExpr (Var _ _ s) resultAlloc jumpTarget = do
  varMap <- gets codegenVarMap
  varAlloc <- case M.lookup s varMap of
                Just varAlloc -> return varAlloc
                Nothing -> codegenError $ "codegenExpr: variable " <> s <> " not found in codegenVarMap"
  case jumpTarget of
    Just jumpTarget -> do
      targetVar <- varTyVarAllocGetVar varAlloc
      ga $ G.jumpLabelSEq jumpTarget (G.var targetVar) trueC
    Nothing -> assignAllocToAlloc resultAlloc varAlloc
-- Tuples can't possibly have a jumpTarget!
codegenExpr (Tuple _ _ exprs) (AllocTuple allocs) _ = sequence_ $ zipWith (\ e a -> codegenExpr e a Nothing)  exprs allocs
codegenExpr t@(Tuple _ _ _) alloc _ = codegenError $ "codegenExpr: tuple " <> show t <> " has an unmatched alloc " <> show alloc
codegenExpr expr varAlloc jumpTarget = do
  isBuiltinOp <- exprIsBuiltinOp expr
  case isBuiltinOp of
    True -> case getExprDepth expr of
              1 -> case jumpTarget of
                     Just jumpTarget -> do
                       case expr of
                         Equal _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           ga $ G.jumpLabelEq jumpTarget oper1 oper2
                         NotEqual _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           ga $ G.jumpLabelNeq jumpTarget oper1 oper2
                         LessThan _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           ga $ G.jumpLabelLt jumpTarget oper1 oper2
                         LessThanEq _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           ga $ G.jumpLabelLe jumpTarget oper1 oper2
                         GreaterThan _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           ga $ G.jumpLabelGt jumpTarget oper1 oper2
                         GreaterThanEq _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           ga $ G.jumpLabelGe jumpTarget oper1 oper2
                         StrictEqual _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           ga $ G.jumpLabelSEq jumpTarget oper1 oper2
                         StrictNEq _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           v1 <- ga $ G.newVar
                           addInstr $ OpI "strictEqual" [G.var v1, oper1, oper2]
                           ga $ G.jumpLabelSEq jumpTarget (G.var v1) falseC
                         _ -> do -- no special support
                           v1 <- ga $ G.newVar
                           codegenExpr expr (AllocVar v1) Nothing
                           ga $ G.jumpLabelSEq jumpTarget (G.var v1) trueC
                     Nothing -> do
                       case expr of
                         Add _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "add" [G.var resultVar, oper1, oper2]
                         Sub _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "sub" [G.var resultVar, oper1, oper2]
                         Mul _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "mul" [G.var resultVar, oper1, oper2]
                         Div _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "div" [G.var resultVar, oper1, oper2]
                         Mod _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "mod" [G.var resultVar, oper1, oper2]
                         Pow _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "pow" [G.var resultVar, oper1, oper2]
                         Equal _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "equal" [G.var resultVar, oper1, oper2]
                         NotEqual _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "notEqual" [G.var resultVar, oper1, oper2]
                         LAnd _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "land" [G.var resultVar, oper1, oper2]
                         LessThan _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "lessThan" [G.var resultVar, oper1, oper2]
                         LessThanEq _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "lessThanEq" [G.var resultVar, oper1, oper2]
                         GreaterThan _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "greaterThan" [G.var resultVar, oper1, oper2]
                         GreaterThanEq _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "greaterThanTq" [G.var resultVar, oper1, oper2]
                         StrictEqual _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "strictEqual" [G.var resultVar, oper1, oper2]
                         StrictNEq _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           v1 <- ga G.newVar
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "strictEqual" [G.var v1, oper1, oper2]
                           addInstr $ OpI "strictEqual" [G.var resultVar, G.var v1, falseC]
                         Shl _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "shl" [G.var resultVar, oper1, oper2]
                         Shr _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "shr" [G.var resultVar, oper1, oper2]
                         LOr _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "lor" [G.var resultVar, oper1, oper2]
                         BOr _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "or" [G.var resultVar, oper1, oper2]
                         BAnd _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "and" [G.var resultVar, oper1, oper2]
                         Xor _ _ expr1 expr2 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           oper2 <- varTyExprLeafOperand expr2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "xor" [G.var resultVar, oper1, oper2]
                         Flip _ _ expr1 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "not" [G.var resultVar, oper1]
                         Not _ _ expr1 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "sub" [G.var resultVar, G.double 1, oper1]
                         Negate _ _ expr1 -> do
                           oper1 <- varTyExprLeafOperand expr1
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "negate" [G.var resultVar, oper1]
                         FunCallExpr _ _ "read" [ExprArg _ cell, ExprArg _ at] -> do
                           resultVar <- varTyVarAllocGetVar varAlloc
                           cellOper <- varTyExprLeafOperand cell
                           atOper <- varTyExprLeafOperand at
                           addInstr $ ReadI (G.var resultVar) cellOper atOper
                         FunCallExpr _ _ "write" [ExprArg _ value, ExprArg _ cell, ExprArg _ at] -> do
                           resultVar <- varTyExprLeafOperand value
                           cellOper <- varTyExprLeafOperand cell
                           atOper <- varTyExprLeafOperand at
                           addInstr $ WriteI resultVar cellOper atOper
                         FunCallExpr _ _ "drawClear" [ExprArg _ r, ExprArg _ g, ExprArg _ b] -> do
                           rOper <- varTyExprLeafOperand r
                           gOper <- varTyExprLeafOperand g
                           bOper <- varTyExprLeafOperand b
                           addInstr $ DrawI (DrawClear rOper gOper bOper)
                         FunCallExpr _ _ "drawColor" [ExprArg _ r, ExprArg _ g, ExprArg _ b, ExprArg _ a ] -> do
                           rOper <- varTyExprLeafOperand r
                           gOper <- varTyExprLeafOperand g
                           bOper <- varTyExprLeafOperand b
                           aOper <- varTyExprLeafOperand a
                           addInstr $ DrawI (DrawColor rOper gOper bOper aOper)
                         FunCallExpr _ _ "drawStroke" [ExprArg _ width ] -> do
                           widthOper <- varTyExprLeafOperand width
                           addInstr $ DrawI (DrawStroke widthOper)
                         FunCallExpr _ _ "drawLine" [ExprArg _ x, ExprArg _ y, ExprArg _ x2, ExprArg _ y2 ] -> do
                           xOper <- varTyExprLeafOperand x
                           yOper <- varTyExprLeafOperand y
                           x2Oper <- varTyExprLeafOperand x2
                           y2Oper <- varTyExprLeafOperand y2
                           addInstr $ DrawI (DrawLine xOper yOper x2Oper y2Oper)
                         FunCallExpr _ _ "drawRect" [ExprArg _ x, ExprArg _ y, ExprArg _ width, ExprArg _ height ] -> do
                           xOper <- varTyExprLeafOperand x
                           yOper <- varTyExprLeafOperand y
                           widthOper <- varTyExprLeafOperand width
                           heightOper <- varTyExprLeafOperand height
                           addInstr $ DrawI (DrawRect xOper yOper widthOper heightOper)
                         FunCallExpr _ _ "drawLineRect" [ExprArg _ x, ExprArg _ y, ExprArg _ width, ExprArg _ height ] -> do
                           xOper <- varTyExprLeafOperand x
                           yOper <- varTyExprLeafOperand y
                           widthOper <- varTyExprLeafOperand width
                           heightOper <- varTyExprLeafOperand height
                           addInstr $ DrawI (DrawLineRect xOper yOper widthOper heightOper)
                         FunCallExpr _ _ "drawPoly" [ExprArg _ x, ExprArg _ y, ExprArg _ sides, ExprArg _ radius, ExprArg _ rotation ] -> do
                           xOper <- varTyExprLeafOperand x
                           yOper <- varTyExprLeafOperand y
                           sidesOper <- varTyExprLeafOperand sides
                           radiusOper <- varTyExprLeafOperand radius
                           rotationOper <- varTyExprLeafOperand rotation
                           addInstr $ DrawI (DrawPoly xOper yOper sidesOper radiusOper rotationOper)
                         FunCallExpr _ _ "drawLinePoly" [ExprArg _ x, ExprArg _ y, ExprArg _ sides, ExprArg _ radius, ExprArg _ rotation ] -> do
                           xOper <- varTyExprLeafOperand x
                           yOper <- varTyExprLeafOperand y
                           sidesOper <- varTyExprLeafOperand sides
                           radiusOper <- varTyExprLeafOperand radius
                           rotationOper <- varTyExprLeafOperand rotation
                           addInstr $ DrawI (DrawLinePoly xOper yOper sidesOper radiusOper rotationOper)
                         FunCallExpr _ _ "drawTriangle" [ExprArg _ x, ExprArg _ y, ExprArg _ x2, ExprArg _ y2, ExprArg _ x3, ExprArg _ y3 ] -> do
                           xOper <- varTyExprLeafOperand x
                           yOper <- varTyExprLeafOperand y
                           x2Oper <- varTyExprLeafOperand x2
                           y2Oper <- varTyExprLeafOperand y2
                           x3Oper <- varTyExprLeafOperand x3
                           y3Oper <- varTyExprLeafOperand y3
                           addInstr $ DrawI (DrawTriangle xOper yOper x2Oper y2Oper x3Oper y3Oper)
                         FunCallExpr _ _ "drawImage" [ExprArg _ x, ExprArg _ y, ExprArg _ image, ExprArg _ size, ExprArg _ rotation ] -> do
                           xOper <- varTyExprLeafOperand x
                           yOper <- varTyExprLeafOperand y
                           imageOper <- varTyExprLeafOperand image
                           sizeOper <- varTyExprLeafOperand size
                           rotationOper <- varTyExprLeafOperand rotation
                           addInstr $ DrawI (DrawImage xOper yOper imageOper sizeOper rotationOper)
                         FunCallExpr _ _ "print" [ExprArg _ output] -> do
                           outputOper <- varTyExprLeafOperand output
                           addInstr $ PrintI outputOper
                         FunCallExpr _ _ "drawFlush" [ExprArg _ to ] -> do
                           toOper <- varTyExprLeafOperand to
                           addInstr $ DrawFlushI toOper
                         FunCallExpr _ _ "printFlush" [ExprArg _ link] -> do
                           linkOper <- varTyExprLeafOperand link
                           addInstr $ PrintFlushI linkOper
                         FunCallExpr _ _ "getLink" [ExprArg _ linkNo] -> do
                           linkNoOper <- varTyExprLeafOperand linkNo
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ GetlinkI (G.var resultVar) linkNoOper
                         FunCallExpr _ _ "controlEnabled" [ExprArg _ obj, ExprArg _ value ] -> do
                           objOper <- varTyExprLeafOperand obj
                           valueOper <- varTyExprLeafOperand value
                           addInstr $ ControlI (CEnabled objOper valueOper)
                         FunCallExpr _ _ "controlShoot" [ExprArg _ obj, ExprArg _ x, ExprArg _ y, ExprArg _ shoot ] -> do
                           objOper <- varTyExprLeafOperand obj
                           xOper <- varTyExprLeafOperand x
                           yOper <- varTyExprLeafOperand y
                           shootOper <- varTyExprLeafOperand shoot
                           addInstr $ ControlI (CShoot objOper xOper yOper shootOper)
                         FunCallExpr _ _ "controlShootp" [ExprArg _ obj, ExprArg _ unit, ExprArg _ shoot ] -> do
                           objOper <- varTyExprLeafOperand obj
                           unitOper <- varTyExprLeafOperand unit
                           shootOper <- varTyExprLeafOperand shoot
                           addInstr $ ControlI (CShootp objOper unitOper shootOper)
                         FunCallExpr _ _ "controlConfigure" [ExprArg _ obj, ExprArg _ val] -> do
                           objOper <- varTyExprLeafOperand obj
                           valOper <- varTyExprLeafOperand val
                           addInstr $ ControlI (CConfigure objOper valOper)
                         FunCallExpr _ _ "radar" [ExprArg _ from, TargetArg _ target1, TargetArg _ target2, TargetArg _ target3, ExprArg _ order, SortArg _ sort] -> do
                           resultVar <- varTyVarAllocGetVar varAlloc
                           fromOper <- varTyExprLeafOperand from
                           orderOper <- varTyExprLeafOperand order
                           addInstr $ RadarI fromOper (G.var target1) (G.var target2) (G.var target3) orderOper (G.var sort) (G.var resultVar)
                         FunCallExpr _ _ "sensor" [ExprArg _ obj, ExprArg _ property ] -> do
                           resultVar <- varTyVarAllocGetVar varAlloc
                           objOper <- varTyExprLeafOperand obj
                           propertyOper <- varTyExprLeafOperand property
                           addInstr $ SensorI (G.var resultVar) objOper propertyOper
                         FunCallExpr _ _ "ubind" [ExprArg _ unitType] -> do
                           unitTypeOper <- varTyExprLeafOperand unitType
                           addInstr $ UBindI unitTypeOper
                         FunCallExpr _ _ "ucontrolStop" [] -> do
                           addInstr $ UControlI UStop
                         FunCallExpr _ _ "ucontrolMove" [ExprArg _ x, ExprArg _ y ] -> do
                           xOper <- varTyExprLeafOperand x
                           yOper <- varTyExprLeafOperand y
                           addInstr $ UControlI (UMove xOper yOper)
                         FunCallExpr _ _ "ucontrolApproach" [ExprArg _ x, ExprArg _ y, ExprArg _ radius ] -> do
                           xOper <- varTyExprLeafOperand x
                           yOper <- varTyExprLeafOperand y
                           radiusOper <- varTyExprLeafOperand radius
                           addInstr $ UControlI (UApproach xOper yOper radiusOper)
                         FunCallExpr _ _ "ucontrolBoost" [ExprArg _ enabled ] -> do
                           enabledOper <- varTyExprLeafOperand enabled
                           addInstr $ UControlI (UBoost enabledOper)
                         FunCallExpr _ _ "ucontrolPathfind" [] -> do
                           addInstr $ UControlI UPathfind
                         FunCallExpr _ _ "ucontrolTarget" [ExprArg _ x, ExprArg _ y, ExprArg _ shoot ] -> do
                           xOper <- varTyExprLeafOperand x
                           yOper <- varTyExprLeafOperand y
                           shootOper <- varTyExprLeafOperand shoot
                           addInstr $ UControlI (UTarget xOper yOper shootOper)
                         FunCallExpr _ _ "ucontrolTargetp" [ExprArg _ unit, ExprArg _ shoot ] -> do
                           unitOper <- varTyExprLeafOperand unit
                           shootOper <- varTyExprLeafOperand shoot
                           addInstr $ UControlI (UTargetp unitOper shootOper)
                         FunCallExpr _ _ "ucontrolItemDrop" [ExprArg _ to, ExprArg _ amount ] -> do
                           toOper <- varTyExprLeafOperand to
                           amountOper <- varTyExprLeafOperand amount
                           addInstr $ UControlI (UItemDrop toOper amountOper)
                         FunCallExpr _ _ "ucontrolItemTake" [ExprArg _ from, ExprArg _ item, ExprArg _ amount ] -> do
                           fromOper <- varTyExprLeafOperand from
                           itemOper <- varTyExprLeafOperand item
                           amountOper <- varTyExprLeafOperand amount
                           addInstr $ UControlI (UItemTake fromOper itemOper amountOper)
                         FunCallExpr _ _ "ucontrolPayDrop" [] -> do
                           addInstr $ UControlI UPayDrop
                         FunCallExpr _ _ "ucontrolPayTake" [ExprArg _ takeUnits ] -> do
                           takeUnitsOper <- varTyExprLeafOperand takeUnits
                           addInstr $ UControlI (UPayTake takeUnitsOper)
                         FunCallExpr _ _ "ucontrolMine" [ExprArg _ x, ExprArg _ y ] -> do
                           xOper <- varTyExprLeafOperand x
                           yOper <- varTyExprLeafOperand y
                           addInstr $ UControlI (UMine xOper yOper)
                         FunCallExpr _ _ "ucontrolFlag" [ExprArg _ value ] -> do
                           valueOper <- varTyExprLeafOperand value
                           addInstr $ UControlI (UFlag valueOper)
                         FunCallExpr _ _ "ucontrolBuild" [ExprArg _ x, ExprArg _ y, ExprArg _ block, ExprArg _ rotation, ExprArg _ config ] -> do
                           xOper <- varTyExprLeafOperand x
                           yOper <- varTyExprLeafOperand y
                           blockOper <- varTyExprLeafOperand block
                           rotationOper <- varTyExprLeafOperand rotation
                           configOper <- varTyExprLeafOperand config
                           addInstr $ UControlI (UBuild xOper yOper blockOper rotationOper configOper)
                         FunCallExpr _ _ "ucontrolGetBlock" [ExprArg _ x, ExprArg _ y ] -> do
                           xOper <- varTyExprLeafOperand x
                           yOper <- varTyExprLeafOperand y
                           [ty, building] <- getDepth1TupleAllocVars varAlloc
                           addInstr $ UControlI (UGetBlock xOper yOper (G.var ty) (G.var building))
                         FunCallExpr _ _ "ucontrolWithin" [ExprArg _ x, ExprArg _ y, ExprArg _ radius ] -> do
                           xOper <- varTyExprLeafOperand x
                           yOper <- varTyExprLeafOperand y
                           radiusOper <- varTyExprLeafOperand radius
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ UControlI (UWithin (G.var resultVar) xOper yOper radiusOper)
                         FunCallExpr _ _ "uradar" [TargetArg _ target1, TargetArg _ target2, TargetArg _ target3, ExprArg _ order, SortArg _ sort] -> do
                           resultVar <- varTyVarAllocGetVar varAlloc
                           orderOper <- varTyExprLeafOperand order
                           addInstr $ URadarI (G.var target1) (G.var target2) (G.var target3) orderOper (G.var sort) (G.var resultVar)
                         FunCallExpr _ _ "ulocateOre" [ExprArg _ ore] -> do
                           oreOper <- varTyExprLeafOperand ore
                           [outX, outY, found] <- getDepth1TupleAllocVars varAlloc
                           addInstr $ ULocateI (ULocateOre oreOper (G.var outX) (G.var outY) (G.var found))
                         FunCallExpr _ _ "ulocateBuilding" [BuildingGroupArg _ buildingGroup, ExprArg _ enemy] -> do
                           enemyOper <- varTyExprLeafOperand enemy
                           [outX, outY, found, building] <- getDepth1TupleAllocVars varAlloc
                           addInstr $ ULocateI (ULocateBuilding (G.var buildingGroup) enemyOper (G.var outX) (G.var outY) (G.var found) (G.var building))
                         FunCallExpr _ _ "ulocateSpawn" [] -> do
                           [outX, outY, found, building] <- getDepth1TupleAllocVars varAlloc
                           addInstr $ ULocateI (ULocateSpawn (G.var outX) (G.var outY) (G.var found) (G.var building))
                         FunCallExpr _ _ "ulocateDamaged" [] -> do
                           [outX, outY, found, building] <- getDepth1TupleAllocVars varAlloc
                           addInstr $ ULocateI (ULocateDamaged (G.var outX) (G.var outY) (G.var found) (G.var building))
                         FunCallExpr _ _ "idiv" [ExprArg _ arg1, ExprArg _ arg2] -> do
                           arg1Oper <- varTyExprLeafOperand arg1
                           arg2Oper <- varTyExprLeafOperand arg2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "idiv" [G.var resultVar, arg1Oper, arg2Oper]
                         FunCallExpr _ _ "max" [ExprArg _ arg1, ExprArg _ arg2] -> do
                           arg1Oper <- varTyExprLeafOperand arg1
                           arg2Oper <- varTyExprLeafOperand arg2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "max" [G.var resultVar, arg1Oper, arg2Oper]
                         FunCallExpr _ _ "min" [ExprArg _ arg1, ExprArg _ arg2] -> do
                           arg1Oper <- varTyExprLeafOperand arg1
                           arg2Oper <- varTyExprLeafOperand arg2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "min" [G.var resultVar, arg1Oper, arg2Oper]
                         FunCallExpr _ _ "atan2" [ExprArg _ arg1, ExprArg _ arg2] -> do
                           arg1Oper <- varTyExprLeafOperand arg1
                           arg2Oper <- varTyExprLeafOperand arg2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "atan2" [G.var resultVar, arg1Oper, arg2Oper]
                         FunCallExpr _ _ "dst" [ExprArg _ arg1, ExprArg _ arg2] -> do
                           arg1Oper <- varTyExprLeafOperand arg1
                           arg2Oper <- varTyExprLeafOperand arg2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "dst" [G.var resultVar, arg1Oper, arg2Oper]
                         FunCallExpr _ _ "noise" [ExprArg _ arg1, ExprArg _ arg2] -> do
                           arg1Oper <- varTyExprLeafOperand arg1
                           arg2Oper <- varTyExprLeafOperand arg2
                           resultVar <- varTyVarAllocGetVar varAlloc
                           addInstr $ OpI "noise" [G.var resultVar, arg1Oper, arg2Oper]
                         FunCallExpr _ _ "abs" [ExprArg _ arg1] -> do
                           resultVar <- varTyVarAllocGetVar varAlloc
                           arg1Oper <- varTyExprLeafOperand arg1
                           addInstr $ OpI "abs" [G.var resultVar, arg1Oper]
                         FunCallExpr _ _ "log" [ExprArg _ arg1] -> do
                           resultVar <- varTyVarAllocGetVar varAlloc
                           arg1Oper <- varTyExprLeafOperand arg1
                           addInstr $ OpI "log" [G.var resultVar, arg1Oper]
                         FunCallExpr _ _ "log10" [ExprArg _ arg1] -> do
                           resultVar <- varTyVarAllocGetVar varAlloc
                           arg1Oper <- varTyExprLeafOperand arg1
                           addInstr $ OpI "log10" [G.var resultVar, arg1Oper]
                         FunCallExpr _ _ "sin" [ExprArg _ arg1] -> do
                           resultVar <- varTyVarAllocGetVar varAlloc
                           arg1Oper <- varTyExprLeafOperand arg1
                           addInstr $ OpI "sin" [G.var resultVar, arg1Oper]
                         FunCallExpr _ _ "cos" [ExprArg _ arg1] -> do
                           resultVar <- varTyVarAllocGetVar varAlloc
                           arg1Oper <- varTyExprLeafOperand arg1
                           addInstr $ OpI "cos" [G.var resultVar, arg1Oper]
                         FunCallExpr _ _ "tan" [ExprArg _ arg1] -> do
                           resultVar <- varTyVarAllocGetVar varAlloc
                           arg1Oper <- varTyExprLeafOperand arg1
                           addInstr $ OpI "tan" [G.var resultVar, arg1Oper]
                         FunCallExpr _ _ "floor" [ExprArg _ arg1] -> do
                           resultVar <- varTyVarAllocGetVar varAlloc
                           arg1Oper <- varTyExprLeafOperand arg1
                           addInstr $ OpI "floor" [G.var resultVar, arg1Oper]
                         FunCallExpr _ _ "ceil" [ExprArg _ arg1] -> do
                           resultVar <- varTyVarAllocGetVar varAlloc
                           arg1Oper <- varTyExprLeafOperand arg1
                           addInstr $ OpI "ceil" [G.var resultVar, arg1Oper]
                         FunCallExpr _ _ "sqrt" [ExprArg _ arg1] -> do
                           resultVar <- varTyVarAllocGetVar varAlloc
                           arg1Oper <- varTyExprLeafOperand arg1
                           addInstr $ OpI "sqrt" [G.var resultVar, arg1Oper]
                         FunCallExpr _ _ "rand" [ExprArg _ arg1] -> do
                           resultVar <- varTyVarAllocGetVar varAlloc
                           arg1Oper <- varTyExprLeafOperand arg1
                           addInstr $ OpI "rand" [G.var resultVar, arg1Oper]
                         _ -> do
                           codegenError $ "codegenExpr: unexpected expr: " <> show expr
              _ -> codegenBuiltinNonDepth1CompExpr expr varAlloc jumpTarget
    False -> do -- must be non-builtin funcall then
      case expr of
        FunCallExpr _ _ funName funArgs -> do
          paramVals <-
            forM funArgs $ \arg ->
              case arg of
                ExprArg _ expr -> do
                  exprTy <- case getType expr of
                              Just ty -> return ty
                              Nothing -> codegenError $ "codegenExpr: " <> show expr <> " does not have type"
                  resultAlloc <- allocVars exprTy
                  codegenExpr expr resultAlloc Nothing
                  return resultAlloc
                _ -> codegenError $ "codegenExpr: unexpected funArgs " <> show funArgs
          funParamRetAlloc <- gets codegenFunParamRetAlloc
          case M.lookup funName funParamRetAlloc of
            Just (paramsAlloc, retValAlloc, retAddrAlloc) -> do
              sequence $ zipWith (\ (_, lhs) rhs -> assignAllocToAlloc lhs rhs) paramsAlloc paramVals
              -- load params
              addExtInstr (ExtFunCallIns funName retAddrAlloc)
              assignAllocToAlloc varAlloc retValAlloc
            Nothing -> codegenError $ "codegenExpr: failed to find function " <> funName <> " in codegenFunParamRetAlloc"
        _ -> codegenError $ "codegenExpr: unexpected expr: " <> show expr
-- codegenStmt stmt lastAlloc breakLab
-- lastAlloc: var alloc for the result of the statement
-- breakLab: should be Just when inside breakable constructs (for, while) (which is the label that break jumps to)
codegenStmt :: Statement SourcePos -> VarAlloc -> Maybe String -> Codegen ()
codegenStmt st@(DeclareStmt pos _ pat tyAnn rhs) _ breakLab = do
  ty <- case tyAnn of
             Just tyAnn -> return tyAnn
             Nothing -> codegenError $ "type not found for the lhs of stmt " <> show st
  patAlloc <- allocPatsInCurScopes pat ty
  mapM_ (\st -> codegenStmt st patAlloc breakLab) rhs
codegenStmt (AssignStmt _ _ pat rhs) _ breakLab = do
  alloc <- lookupPatInsVars pat
  codegenStmt rhs alloc breakLab
codegenStmt (BlockStmt _ _ blk) lastAlloc breakLab = codegenBlock blk lastAlloc breakLab
codegenStmt (ExprStmt _ _ expr) lastAlloc _ = codegenExpr expr lastAlloc Nothing
codegenStmt (EndStmt _ _) _ _ = addInstr $ EndI
codegenStmt (ForStmt _ _ init cond step blk) _ breakLab = 
  newScope $ do
    codegenStmt init (AllocTuple []) breakLab
    loopStart <- addNewLab
    loopEnd <- newLab
    codegenExpr (Not noLoc (Just VarTy) cond) (AllocTuple []) (Just loopEnd)
    codegenBlock blk (AllocTuple []) (Just loopEnd)
    ga $ G.jumpLabel loopStart
    addLab loopEnd
codegenStmt (IfThenStmt _ _ cond cons) _ breakLab = do
   loopEnd <- newLab
   codegenExpr (Not noLoc (Just VarTy) cond) (AllocTuple []) (Just loopEnd)
   codegenBlock cons (AllocTuple []) breakLab
   addLab loopEnd
codegenStmt (IfThenElseStmt _ _ expr cons alt) lastAlloc breakLab = do
  labCons <- newLab
  labEnd <- newLab
  codegenExpr expr (AllocTuple []) (Just labCons)
  codegenBlock alt lastAlloc breakLab
  ga $ G.jumpLabel labEnd
  addLab labCons
  codegenBlock cons lastAlloc breakLab
  addLab labEnd
codegenStmt (WhileStmt _ _ cond blk) _ _ = do
  loopStart <- addNewLab
  loopEnd <- newLab
  codegenExpr (Not noLoc (Just VarTy) cond) (AllocTuple []) (Just loopEnd)
  codegenBlock blk (AllocTuple []) (Just loopEnd)
  ga $ G.jumpLabel loopStart
  addLab loopEnd
codegenStmt (BreakStmt _ _) _ breakLab = case breakLab of
                                           Just breakLab -> ga $ G.jumpLabel breakLab
                                           Nothing -> codegenError $ "codegenStmt: break label not found at break instruction"
codegenStmt (ReturnStmt _ _ rhs) _ breakLab = do
  CodegenState{codegenFunParamRetAlloc,codegenFunName} <- get
  case M.lookup codegenFunName codegenFunParamRetAlloc of
    Just (_, retValAlloc, retAddrAlloc) -> do
      mapM_ (\st -> codegenStmt st retValAlloc breakLab) rhs
      case codegenFunName == "main" of
        True -> addInstr $ EndI
        False -> addExtInstr $ ExtFunRet $ G.var retAddrAlloc
    Nothing -> codegenError $ "codegenStmt: failed to locate function " <> codegenFunName <> " in codegenFunParmRetAlloc"

codegenStmts :: [Statement SourcePos] -> VarAlloc -> Maybe String -> Codegen ()
codegenStmts [x] lastAlloc breakLab = codegenStmt x lastAlloc breakLab
codegenStmts (ReturnStmt loc ty rhs:_) lastAlloc breakLab = codegenStmt (ReturnStmt loc ty rhs) lastAlloc breakLab
codegenStmts (x:xs) lastAlloc breakLab = do
  codegenStmt x (AllocTuple []) breakLab
  codegenStmts xs lastAlloc breakLab
codegenStmts [] _ _ = return ()

codegenBlock :: Block SourcePos -> VarAlloc -> Maybe String -> Codegen ()
codegenBlock blk lastAlloc breakLab = do
  newScope $ do
    codegenStmts (stmts blk) lastAlloc breakLab

loadFuncParamsToCurScope :: String -> Codegen ()
loadFuncParamsToCurScope funName = do
  funParamRetAlloc <- gets codegenFunParamRetAlloc
  case M.lookup funName funParamRetAlloc of
    Just (paramsAlloc, _, _) -> mapM_ (\ (paramName, paramAlloc) -> loadAllocedVarInCurScope paramName paramAlloc) paramsAlloc
    Nothing -> codegenError $ "loadFuncParamsToCurScope: failed to find " <> funName <> " in codegenFunParamRetAlloc"


codegenFunction :: Function SourcePos -> Codegen (Endo [ExtInstr])
codegenFunction f = do
  clearIns
  modify $ \s -> s { codegenFunName = functionName f }
  newScope $ do
    loadFuncParamsToCurScope (functionName f)
    let stmtsWithRet = (stmts $ functionBody f) ++ [ReturnStmt noLoc (Just VarTy) Nothing]
        bodyWithRet = (functionBody f) { stmts = stmtsWithRet }
    codegenBlock bodyWithRet (AllocTuple []) Nothing
  gets codegenIns


codegenFunctions :: [Function SourcePos] -> Codegen (Map String (Endo [ExtInstr]))
codegenFunctions funcs = do
  funcSigMap <- gets codegenFuncSigMap
  funParamRetAlloc <- flip mapM funcSigMap $ \ (params, retTy) -> do
                        paramsAlloc <-
                           forM params $ \ (paramName, _, paramTy) -> do
                                  paramAlloc <- allocVarsWithHint paramName paramTy
                                  return (paramName, paramAlloc)
                        retAlloc <- allocVarsWithHint "retVal" retTy
                        retAddrAlloc <- ga $ G.newVarWithHint "retAddr"
                        return (paramsAlloc, retAlloc, retAddrAlloc)
  funEntryMap <- fmap M.fromList $ forM funcs $ \f -> do
      funEntryLab <- ga G.newLab
      return (functionName f, funEntryLab)
  
  modify $ \s -> s { codegenFunParamRetAlloc = funParamRetAlloc
                   , codegenFuncEntryMap = funEntryMap }
  funcsIns <-
     mapM (\f -> do
              r <- codegenFunction f
              return (functionName f, r)) funcs
  return $ M.fromList funcsIns

codegenProgram :: [String] -> [String]
    -> Map String BuiltinFunction
    -> Map String ([(String, SourcePos, Types)], Types)
    -> Program SourcePos
    -> Codegen (Map String (Endo [ExtInstr]))
codegenProgram inputVars linkConstants builtinFuncMap funcSigMap p = do
  
  modify (\s -> s { codegenInsVarSet = S.fromList (inputVars <> linkConstants)
                  , codegenBuiltinFuncMap = builtinFuncMap
                  , codegenVarMap = M.fromList $ (map (\s -> (s, AllocVar s))) (inputVars <> linkConstants)
                  , codegenFuncSigMap = funcSigMap })
  codegenFunctions (functions p)  
{-# LANGUAGE NamedFieldPuns #-}
module MLogic.HighLevel.TC where

import Control.Monad.Error
import Control.Monad.Trans
import Control.Monad.State

import Data.Maybe (fromMaybe)
import Data.List (foldl')
import MLogic.HighLevel.Types
import MLogic.HighLevel.TC.Types
import MLogic.HighLevel.Types
import Text.Parsec (SourcePos)

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

import MLogic.HighLevel.BuiltinFunctions

tcError :: TCMsg a => a -> TC b
tcError msg = throwError ("error: " <> tcMsg msg)

tcWarn :: TCMsg a => a -> TC ()
tcWarn msg = do
  logger <- gets tcLogger
  liftIO $ logger ("warning: " <> tcMsg msg)



tcInputVars :: [(SourcePos, String, Maybe CL)] -> TC ()
tcInputVars ((pos, inputVarName, Just (TupleLit lits)):xs)
  = tcError $ [ "invalid input variable assignment to " <> inputVarName
              , "at " <> show pos ]
tcInputVars ((pos, inputVarName, lit):xs) = do
  tcCheckTopLevelNames inputVarName pos "input variable"
  st@(TCState { tcVarMap, tcInputVarSet, tcInputVarMap }) <- get
  put st { tcInputVarSet = S.insert inputVarName tcInputVarSet
         , tcInputVarMap = M.insert inputVarName lit tcInputVarMap
         , tcVarMap = M.insert inputVarName (VarTy, "input variable", pos, 0) tcVarMap
         }
  insertTopLevelName inputVarName (Just pos) "input variable" True
  tcInputVars xs
tcInputVars [] = return ()

insertFunctionName :: String -> SourcePos -> TC ()
insertFunctionName name pos = do
  tcCheckTopLevelNames name pos "function"
  insertTopLevelName name (Just pos) "function" False

newScope :: TC a -> TC a
newScope p = do
  currentScopeLevel <- gets tcScopeLevel
  currentVarMap <- gets tcVarMap
  modify (\s -> s { tcScopeLevel = currentScopeLevel + 1 })
  r <- p
  modify (\s -> s { tcScopeLevel = currentScopeLevel, tcVarMap = currentVarMap })
  return r

withBreakable :: TC a -> TC a
withBreakable p = do
  tcCurBreakable <- gets tcInBreakable
  modify $ \s -> s { tcInBreakable = True }
  r <- p
  modify $ \s -> s { tcInBreakable = tcCurBreakable }
  return r

insertNonTopLevelVar :: String -> SourcePos -> Types -> String -> TC ()
insertNonTopLevelVar varName pos ty varDesc = do
  TCState {tcTopLevelReservedNamesMap, tcVarMap, tcScopeLevel} <- get
  let insertedVarMap = M.insert varName (ty, varDesc, pos, tcScopeLevel) tcVarMap
  case M.lookup varName tcTopLevelReservedNamesMap of
    Just (varDesc2, pos2) -> tcError $ [ varDesc <> " " <> varName <> " (at " <> show pos <> ") "
                                       <> "conflicts with the name of a reserved " <> varDesc2
                                       <> " (" <> fromMaybe "no source loc" (fmap show pos2) <> ")" ]
    Nothing -> case M.lookup varName tcVarMap of
                 Just (_, oldVarDesc, oldVarPos, oldVarLevel) -> do
                      tcWarn $ [ varDesc <> " " <> varName <> " (at " <> show pos <> ") "
                                       <> "shadows the name of an existing " <> oldVarDesc
                                       <> " (" <> show oldVarPos <> ")" ]
                      modify (\s -> s { tcVarMap = insertedVarMap })
                 Nothing -> modify (\s -> s { tcVarMap = insertedVarMap })

-- checks that a pattern is well-formed
-- i.e. all variables in the pattern are unique
patWellFormed :: Pat SourcePos -> TC ()
patWellFormed p = void (go p S.empty)
  where
    go :: Pat SourcePos -> Set String -> TC (Set String)
    go (VarPat pos name) set = case S.member name set of
                                 True -> tcError $ [ "repeated pattern variable " <> name <> " at " <> show pos ]
                                 False -> return (S.insert name set)
    go (TuplePat pos pats) set = foldl' (\acc elem -> acc >>= go elem) (return set) pats
    go (IgnorePat pos) set = return set
-- checks compatibility of pattern w/ type
-- a VarPat/IgnorePat is compatible with all possible types
-- a TuplePat is compatible with a type iff the type is a TupleTy
-- and all corresponding subpats and subtypes are compatible
-- returns the type of the variables in the pattern
patCompatTy :: Pat SourcePos -> Types -> TC [(String, SourcePos, Types)]
patCompatTy (VarPat pos name) ty = return [(name, pos, ty)]
patCompatTy (IgnorePat pos) ty = return []
patCompatTy p@(TuplePat pos pats) VarTy = tcError $ [ "incompatible pattern type at " <> show pos
                                                  , "the pattern is supposed to have type " <> show VarTy
                                                     <> ", but this type is incompatible with pattern " <> show p ]
patCompatTy p@(TuplePat pos pats) ty@(TupleTy tys) =
  case length pats == length tys of
    True -> do
      results <- sequence $ zipWith patCompatTy pats tys
      return $ concat results
    False -> tcError $ [ "incompatible pattern type at " <> show pos
                       , "the pattern is supposed to have type " <> show ty
                            <> ", but this type is incompatible with pattern " <> show p ]

-- checks that all pattern variables are in scope
checkPatVarsInScope :: Pat SourcePos -> TC ()
checkPatVarsInScope (VarPat pos name) = do
  TCState {tcVarMap} <- get
  case M.lookup name tcVarMap of
    Just _ -> return ()
    Nothing -> tcError $ [ "pattern variable " <> name <> " at " <> show pos <> " is not declared" ]
checkPatVarsInScope (IgnorePat _) = return ()
checkPatVarsInScope (TuplePat _ pats) = sequence_ $ map checkPatVarsInScope pats

-- checks that a variable is in scope and has the specified type
checkVarType :: String -> SourcePos -> String -> Types -> TC ()
checkVarType var pos varDesc ty = do
  varMap  <- gets tcVarMap
  case M.lookup var varMap of
    Just (ty2, desc2, pos2, level) ->
      when (ty /= ty2) $
        tcError $ [ varDesc <> " " <> var <> " at " <> show pos <> " is supposed to have type " <> show ty
                  , " but an earlier declaration of a(n) " <> desc2 <> " at " <> show pos <> " has type " <> show ty2 ]
    Nothing -> tcError $ [ varDesc <> " " <> var <> " at " <> show pos <> " is not declared " ]


getTCedBlockTy :: Block SourcePos -> TC Types
getTCedBlockTy blk = case blockTy blk of
                       Just ty -> return ty
                       Nothing -> tcError $ [ "internal error at getTCedBlockTy: type checked block does not have type" ]


getTCedStmtTy :: Statement SourcePos -> TC Types
getTCedStmtTy st = case getType st of
                     Just ty -> return ty
                     Nothing -> tcError $ [ "internal error at getTCedStmtTy: type checked statment does not have type" ]


getTCedExprTy :: Expr SourcePos -> TC Types
getTCedExprTy expr = case getType expr of
                       Just ty -> return ty
                       Nothing -> tcError $ [ "internal error at getTCedExprTy: type checked expr does not have type" ]

tcBinOp :: (SourcePos -> Maybe Types -> Expr SourcePos -> Expr SourcePos -> Expr SourcePos)
        -> SourcePos -> Expr SourcePos -> Expr SourcePos -> Map Level (Set String) -> TC (Expr SourcePos)
tcBinOp op pos expr1 expr2 varInit = do
  checkedExpr1 <- tcExpr expr1 varInit
  checkedExpr1Ty <- getTCedExprTy checkedExpr1
  checkedExpr2 <- tcExpr expr2 varInit
  checkedExpr2Ty <- getTCedExprTy checkedExpr2
  when (checkedExpr1Ty /= VarTy) $ do
    tcError $ [ "unexpected expression (" <> show expr1 <> ") at " <> show (getPos checkedExpr1)
              , "expected Var, got " <> show checkedExpr1Ty ]
  when (checkedExpr2Ty /= VarTy) $ do
    tcError $ [ "unexpected expression (" <> show expr2 <> ") at " <> show (getPos checkedExpr2)
              , "expected Var, got " <> show checkedExpr2Ty ]
  return (op pos (Just VarTy) checkedExpr1 checkedExpr2)


tcUnaryOp :: (SourcePos -> Maybe Types -> Expr SourcePos -> Expr SourcePos)
        -> SourcePos -> Expr SourcePos -> Map Level (Set String) -> TC (Expr SourcePos)
tcUnaryOp op pos expr1 varInit = do
  checkedExpr1 <- tcExpr expr1 varInit
  checkedExpr1Ty <- getTCedExprTy checkedExpr1
  when (checkedExpr1Ty /= VarTy) $ do
    tcError $ [ "unexpected expression (" <> show expr1 <> ") at " <> (show (getPos checkedExpr1))
              , "expected Var, got " <> show checkedExpr1Ty ]
  return (op pos (Just VarTy) checkedExpr1)


-- check funArg type for non builtin function calls
checkFunArgTy ::  String -> Map Level (Set String) -> (String, SourcePos, Types) -> FunCallArg SourcePos -> TC (FunCallArg SourcePos)
checkFunArgTy funName varInit (paramName, paramPos, paramTy) (ExprArg pos expr) = do
  checkedExpr <- tcExpr expr varInit
  exprTy <- getTCedExprTy checkedExpr
  when (paramTy /= exprTy) $ do
    tcError $ [ "argument (" <> show expr <> ") at " <> show pos <> " has type " <> show exprTy <> ", but the parameter "
                  <>  paramName <> " to function " <> paramName <> " declared at " <> show paramPos
                  <> " expects a " <> show paramTy ]
  return (ExprArg pos checkedExpr)
checkFunArgTy funName varInit (paramName, paramPos, paramTy) arg = tcError $ [ "argument (" <> show arg <> ") at " <> show (getPos arg) <> " to function " <> funName <> " cannot be used to call a non-builtin function" ]


-- check funArg type for builtin function calls
checkBuiltinFunArgTy :: String -> Map Level (Set String) -> (String, BuiltinTy) -> FunCallArg SourcePos -> TC (FunCallArg SourcePos)
checkBuiltinFunArgTy funName varInit (paramName, BuiltinVar) (ExprArg pos expr) = do
  checkedExpr <- tcExpr expr varInit
  checkedExprTy <- getTCedExprTy checkedExpr
  when (checkedExprTy /= VarTy) $ do
    tcError $ [ "argument (" <> show expr <> ") at " <> show pos <> " to function " <> funName <> " has type " <> show checkedExprTy <> ", but the argument is expected to have type " <> show VarTy ]
  return (ExprArg pos checkedExpr)
checkBuiltinFunArgTy funName varInit (paramName, BuiltinTarget) arg@(TargetArg _ _) = return arg
checkBuiltinFunArgTy funName varInit (paramName, BuiltinSort) arg@(SortArg _ _) = return arg
checkBuiltinFunArgTy funName varInit (paramName, BuiltinBuildingGroup) arg@(BuildingGroupArg _ _) = return arg
checkBuiltinFunArgTy funName varInit (paramName, bltTy) arg
   = tcError $ [ "argument (" <> show arg <> ") at " <> show (getPos arg) <> " is incompatible with the type "
                 <> show bltTy <> " of the builtin parameter " <> paramName <> " of function " <> funName ]

getCLTy :: CL -> Types
getCLTy (AtConstant _) = VarTy
getCLTy (DoubleLit _) = VarTy
getCLTy (StrLit _) = VarTy
getCLTy (TupleLit cls) = TupleTy (map getCLTy cls)
getCLTy TrueL = VarTy
getCLTy FalseL = VarTy
getCLTy Null = VarTy

lookupVarInitialized :: String -> Map Level (Set String) -> TC Bool
lookupVarInitialized varName varInit = do
  TCState{tcVarMap} <- get
  case M.lookup varName tcVarMap of
    Just (_,_,_,level) ->
      case M.lookup level varInit of
        Nothing -> return False
        Just s -> return $ S.member varName s
    Nothing -> return False

tcExpr :: Expr SourcePos -> Map Level (Set String) -> TC (Expr SourcePos)
tcExpr (Add pos _ expr1 expr2) varInit = tcBinOp Add pos expr1 expr2 varInit
tcExpr (Sub pos _ expr1 expr2) varInit = tcBinOp Sub pos expr1 expr2 varInit
tcExpr (Mul pos _ expr1 expr2) varInit = tcBinOp Mul pos expr1 expr2 varInit
tcExpr (Div pos _ expr1 expr2) varInit = tcBinOp Div pos expr1 expr2 varInit
tcExpr (Mod pos _ expr1 expr2) varInit = tcBinOp Mod pos expr1 expr2 varInit
tcExpr (Pow pos _ expr1 expr2) varInit = tcBinOp Pow pos expr1 expr2 varInit
tcExpr (Equal pos _ expr1 expr2) varInit = tcBinOp Equal pos expr1 expr2 varInit
tcExpr (NotEqual pos _ expr1 expr2) varInit = tcBinOp NotEqual pos expr1 expr2 varInit
tcExpr (LAnd pos _ expr1 expr2) varInit = tcBinOp LAnd pos expr1 expr2 varInit
tcExpr (LessThan pos _ expr1 expr2) varInit = tcBinOp LessThan pos expr1 expr2 varInit
tcExpr (LessThanEq pos _ expr1 expr2) varInit = tcBinOp LessThanEq pos expr1 expr2 varInit
tcExpr (GreaterThan pos _ expr1 expr2) varInit = tcBinOp GreaterThan pos expr1 expr2 varInit
tcExpr (GreaterThanEq pos _ expr1 expr2) varInit = tcBinOp GreaterThanEq pos expr1 expr2 varInit
tcExpr (StrictEqual pos _ expr1 expr2) varInit = tcBinOp StrictEqual pos expr1 expr2 varInit
tcExpr (StrictNEq pos _ expr1 expr2) varInit = tcBinOp StrictNEq pos expr1 expr2 varInit
tcExpr (Shl pos _ expr1 expr2) varInit = tcBinOp Shl pos expr1 expr2 varInit
tcExpr (Shr pos _ expr1 expr2) varInit = tcBinOp Shr pos expr1 expr2 varInit
tcExpr (LOr pos _ expr1 expr2) varInit = tcBinOp LOr pos expr1 expr2 varInit
tcExpr (BOr pos _ expr1 expr2) varInit = tcBinOp BOr pos expr1 expr2 varInit
tcExpr (BAnd pos _ expr1 expr2) varInit = tcBinOp BAnd pos expr1 expr2 varInit
tcExpr (Xor pos _ expr1 expr2) varInit = tcBinOp Xor pos expr1 expr2 varInit
tcExpr (Flip pos _ expr1) varInit = tcUnaryOp Flip pos expr1 varInit
tcExpr (Not pos _ expr1) varInit = tcUnaryOp Not pos expr1 varInit
tcExpr (Negate pos _ expr1) varInit = tcUnaryOp Negate pos expr1 varInit
tcExpr (FunCallExpr pos _ funName args) varInit = do
  TCState{tcFunctionSigMap, tcBuiltinFunctionMap} <- get
  let lenCheck paramLen = when (paramLen /= length args) $ do
                            tcError $ [ "function " <> funName <> " has "
                                         <> show paramLen <> " parameter(s), but only "
                                         <> show (length args) <> " argument(s) are supplied at " <> show pos ]
  case M.lookup funName tcFunctionSigMap of
    Just (params, retTy) -> do
      lenCheck (length params)
      checkedArgs <- sequence $ zipWith (checkFunArgTy funName varInit) params args
      return (FunCallExpr pos (Just retTy) funName checkedArgs)
    Nothing -> case M.lookup funName tcBuiltinFunctionMap of
                 Just builtinFunction -> do
                   lenCheck (length $ builtinFunctionParams builtinFunction)
                   checkedArgs <- sequence $ zipWith (checkBuiltinFunArgTy funName varInit) (builtinFunctionParams builtinFunction) args
                   return (FunCallExpr pos (Just $ builtinFunctionRetType builtinFunction) funName checkedArgs)
                 Nothing -> tcError $ [ "function " <> funName <> " at " <> show pos <> " is not a declared function nor a builtin function" ]
tcExpr (Tuple pos _ exprs) varInit = do
  checkedExprs <- mapM (\e -> tcExpr e varInit) exprs
  checkedExprTys <- mapM getTCedExprTy checkedExprs
  return (Tuple pos (Just (TupleTy checkedExprTys)) checkedExprs)
tcExpr (Lit pos _ cl) varInit = return (Lit pos (Just $ getCLTy cl) cl)
tcExpr (Var pos _ name) varInit = do
  TCState{tcVarMap} <- get
  case M.lookup name tcVarMap of
    Just (ty, _, _, _) -> do
      isInitialized <- lookupVarInitialized name varInit
      when (not isInitialized) $ do
        tcError $ "use of potentially uninitialized variable " <> name <> " at " <> show pos
      return $ Var pos (Just ty) name
    Nothing -> tcError $ "use of undeclared variable " <> name <> " at " <> show pos



varInitAddVars :: Level -> [String] -> Map Level (Set String) -> Map Level (Set String)
varInitAddVars l xs map
  = M.insertWith S.union l (S.fromList xs) map


-- add accessed variables w.r.t to the level of the variables in the lexical map tcVarMap
varInitAddVarsWithVarLevel :: [String] -> Map Level (Set String) -> TC (Map Level (Set String))
varInitAddVarsWithVarLevel (x:xs) varInit = do
  varMap <- gets tcVarMap
  case M.lookup x varMap of
    Just (_, _, _, level) ->
      let newVarInit = M.insertWith S.union level (S.singleton x) varInit
      in varInitAddVarsWithVarLevel xs newVarInit
    Nothing -> tcError $ "internal error: variable " <> x <> " not found in varInitAddVarsWithVarLevel"
varInitAddVarsWithVarLevel [] varInit = return varInit

varInitDeleteVars :: Level -> [String] -> Map Level (Set String) -> Map Level (Set String)
varInitDeleteVars l xs map
  = M.insertWith (\new ori -> S.difference ori new)  l (S.fromList xs) map

-- remove vars from varInit *before* exiting a scope
varInitDeScope :: Map Level (Set String) -> TC (Map Level (Set String))
varInitDeScope varInit = do
  curScope <- gets tcScopeLevel
  return $ M.delete curScope varInit


-- retTy is the expected return type
-- varInit is the map of variables that are currently initialized
tcStmt :: Statement SourcePos -> Types -> Map Level (Set String) -> TC (Statement SourcePos, Map Level (Set String))
tcStmt (DeclareStmt pos _ pat tyAnn rhsStmt) retTy varInit = do
  (checkedRhs, ty, rhsVarInit) <-
        case rhsStmt of
          Just rhsStmt -> do
            (checkedRhs, newVarInit) <- tcStmt rhsStmt retTy varInit
            rhsTy <- getTCedStmtTy checkedRhs
            case tyAnn of
              Just tyAnn -> do
                when (rhsTy /= tyAnn) $
                  tcError $ [ "declaration statement at " <> show pos <> " has an unsatisfiable type annotation"
                            , "right hand side has type " <> show rhsTy <> ", but the type annotation is " <> show tyAnn
                            ]
                return (Just checkedRhs, rhsTy, newVarInit)
              Nothing -> return (Just checkedRhs, rhsTy, newVarInit)
          Nothing ->
             case tyAnn of
               Just tyAnn -> return (Nothing, tyAnn, varInit)
               Nothing -> tcError $ [ "cannot determine type for pattern variable " <> show pat <> " at " <> show pos
                                    , "suggestion: add a type annotation or a right hand side statement" ]
  --
  patWellFormed pat
  -- check pat compatibility w/ ty
  patVarTys <- patCompatTy pat ty
  mapM_ (\ (name, pos, ty) -> insertNonTopLevelVar name pos ty "pattern variable") patVarTys
  currentScope <- gets tcScopeLevel
  -- if there is an rhs, the introduced vars are initialized
  -- if there isn't, then the introduced vars are not initialized
  case checkedRhs of
    Just _ ->
      let newVarInit = varInitAddVars currentScope (getPatVars pat) rhsVarInit
      in return $ (DeclareStmt pos (Just (TupleTy [])) pat (Just ty) checkedRhs, newVarInit)
    Nothing ->
      let newVarInit = varInitDeleteVars currentScope (getPatVars pat) rhsVarInit
      in return $ (DeclareStmt pos (Just (TupleTy [])) pat (Just ty) checkedRhs, newVarInit)
tcStmt (AssignStmt pos _ pat rhs) retTy varInit = do
  (checkedRhs, rhsVarInit) <- tcStmt rhs retTy varInit
  rhsTy <- getTCedStmtTy checkedRhs
  patWellFormed pat
  checkPatVarsInScope pat
  patVarTy <- patCompatTy pat rhsTy
  -- make sure all patVars are in scope and have the right types in the lexical env
  mapM_ (\ (name', pos', ty') -> checkVarType name' pos' "pattern variable" ty') patVarTy
  resultVarInit <- varInitAddVarsWithVarLevel (getPatVars pat) rhsVarInit
  return (AssignStmt pos (Just (TupleTy [])) pat checkedRhs, resultVarInit)
tcStmt (BlockStmt pos _ block) retTy varInit = do
  (checkedBlock, newVarInit) <- tcBlock block retTy varInit
  return (BlockStmt pos (blockTy checkedBlock) checkedBlock, newVarInit)
tcStmt (ExprStmt pos _ expr) retTy varInit = do
  checkedExpr <- tcExpr expr varInit
  return (ExprStmt pos (getType checkedExpr) checkedExpr, varInit)
tcStmt (EndStmt pos _) retTy varInit = return (EndStmt pos (Just $ TupleTy []), varInit)
tcStmt (ForStmt pos _ init cond step blk) retTy varInit = do
  newScope $ withBreakable $ do
    (checkedInit, initVarInit) <- tcStmt init retTy varInit
    checkedCond <- tcExpr cond initVarInit
    checkedCondTy <- getTCedExprTy checkedCond
    when (checkedCondTy /= VarTy) $ do
      tcError $ [ "for condition " <> show cond <> " at " <> show (getPos cond) <> " should have type " <> show VarTy
                , " but has type " <> show checkedCondTy ]
    (checkedStep, _) <- tcStmt step retTy initVarInit -- var access result from step & block *should not be used*
    (checkedBlk, _) <- tcBlock blk retTy initVarInit  -- they are only conditionally executed
    -- only thing that a for loop can possibly initialize in the original scope
    -- is the variables that are assigned to in init
    -- we have to descope initVarInit because we are exiting the current scope
    resultVarInit <- varInitDeScope initVarInit
    return (ForStmt pos (Just $ TupleTy []) checkedInit checkedCond checkedStep checkedBlk, resultVarInit)
tcStmt (IfThenStmt pos _ cond cons) retTy varInit = do
  checkedCond <- tcExpr cond varInit
  checkedCondTy <- getTCedExprTy checkedCond
  when (checkedCondTy /= VarTy) $ do
    tcError $ [ "IfThen condition " <> show cond <> " at " <> show (getPos cond) <> " should have type " <> show VarTy
              , " but has type " <> show checkedCondTy ]
  (checkedCons, _) <- tcBlock cons retTy varInit
  -- IfThenStmt does not change whether or not a variable is initialized
  return (IfThenStmt pos (Just $ TupleTy []) checkedCond checkedCons, varInit)
tcStmt (IfThenElseStmt pos _ cond cons alt) retTy varInit = do
  checkedCond <- tcExpr cond varInit
  checkedCondTy <- getTCedExprTy checkedCond
  when (checkedCondTy /= VarTy) $ do
    tcError $ [ "IfThenElse condition " <> show cond <> " at " <> show (getPos cond) <> " should have type " <> show VarTy
              , " but has type " <> show checkedCondTy ]
  (checkedCons, consVarInit) <- tcBlock cons retTy varInit
  checkedConsTy <- getTCedBlockTy checkedCons
  (checkedAlt, altVarInit) <- tcBlock alt retTy varInit
  checkedAltTy <- getTCedBlockTy checkedAlt
  when (checkedConsTy /= checkedAltTy) $ do
    tcError $ [ "IfThenElse statement at " <> show pos <> " has unmatched branch types: "
                 <> "then branch has type " <> show checkedConsTy <> " while the else branch has type " <> show checkedAltTy ]
  -- variables that are initialized by an IFThenElseStmt are the variables that are written to by *both* branches
  let resultVarInit = M.intersectionWith S.intersection consVarInit altVarInit
  return (IfThenElseStmt pos (Just checkedConsTy) checkedCond checkedCons checkedAlt, resultVarInit)
tcStmt (WhileStmt pos _ cond block) retTy varInit = do
  checkedCond <- tcExpr cond varInit
  checkedCondTy <- getTCedExprTy checkedCond
  when (checkedCondTy /= VarTy) $ do
    tcError $ [ "while loop condition " <> show cond <> " at " <> show (getPos cond) <> " should have type " <> show VarTy
              , " but has type " <> show checkedCondTy ]
  (checkedBlock, _) <- withBreakable $ tcBlock block retTy varInit
  -- a while statement does not change variable initialization in the original scope as the body is only conditionally executed
  return (WhileStmt pos (Just $ TupleTy []) checkedCond checkedBlock, varInit)
tcStmt (BreakStmt pos _) _ varInit = do
  breakable <- gets tcInBreakable
  when (breakable == False) $
    tcError $ "break statement at " <> show pos <> " is not inside a for/while loop"
  return (BreakStmt pos (Just $ TupleTy []), varInit)
tcStmt (ReturnStmt pos _ rhs) retTy varInit = do
  checkedRhsWithVarInit <- sequence $ fmap (\st -> tcStmt st retTy varInit) rhs
  (checkedRhs, checkedRhsTy, rhsVarInit)
      <- case checkedRhsWithVarInit of
             Just (checkedRhs, rhsVarInit) -> do
               checkedRhsTy <- getTCedStmtTy checkedRhs
               return (Just checkedRhs, checkedRhsTy, rhsVarInit)
             Nothing -> return (Nothing, TupleTy [], varInit)
  when (checkedRhsTy /= retTy) $ do
    tcError $ [ "return statement at " <> show pos <> " returns type " <> show checkedRhsTy
                 <> " but is expected to return type " <> show retTy ]
  return (ReturnStmt pos (Just $ TupleTy []) checkedRhs, rhsVarInit)


-- returns the typechecked statments, the type of the last statement, and variable initialization map
tcStmts :: [Statement SourcePos] -> Types -> Map Level (Set String) -> TC ([Statement SourcePos], Types, Map Level (Set String))
tcStmts [x] retTy varInit = do
  (st, newVarInit) <- tcStmt x retTy varInit
  ty <- getTCedStmtTy st
  return ([st], ty, newVarInit)
tcStmts (x:xs) retTy varInit = do
  (st, newVarInit) <- tcStmt x retTy varInit
  (rest, ty, newNewVarInit) <- tcStmts xs retTy newVarInit
  return (st:rest, ty, newNewVarInit)
tcStmts [] retTy varInit = return ([], TupleTy [], varInit)

tcBlock :: Block SourcePos -> Types -> Map Level (Set String) -> TC (Block SourcePos, Map Level (Set String))
tcBlock (Block{stmts}) retTy varInit
  = newScope $ do
      (checkedStmts, ty, stmtsVarInit) <- tcStmts stmts retTy varInit
      descopedVarInit <- varInitDeScope stmtsVarInit
      return (Block {stmts = checkedStmts, blockTy = Just ty}, descopedVarInit)

-- checks that function parameters are unique
tcFunctionParamUnique :: [(String, SourcePos, Types)] -> TC ()
tcFunctionParamUnique xs = go S.empty xs
  where
    go :: Set String -> [(String, SourcePos, Types)] -> TC ()
    go s ((name, pos, _):xs) = case S.member name s of
                                 True -> tcError [ "function parameter name " <> name <> " at " <> show pos <> " conflicts with a previous parameter "
                                                 , "function parameter names should be unique" ]
                                 False -> go (S.insert name s) xs
    go s [] = return ()

hasTopLevelRetStmt :: [Statement SourcePos] -> Bool
hasTopLevelRetStmt ((ReturnStmt _ _ _):_) = True
hasTopLevelRetStmt (_:xs) = hasTopLevelRetStmt xs
hasTopLevelRetStmt [] = False

tcFunctionBody :: Set String -> Function SourcePos -> TC (Function SourcePos)
tcFunctionBody topLevelVars f@(Function {functionName, functionParams, functionRetType, functionBody, functionLoc})
  = newScope $ do
       mapM_ (\ (paramName, paramPos, paramType)
                             -> insertNonTopLevelVar paramName paramPos paramType "function parameter") functionParams
       currentScope <- gets tcScopeLevel
       let retTy = case functionRetType of
                     Just ty -> ty
                     Nothing -> TupleTy []
           varInit = M.fromList [ (currentScope, (S.fromList $ map (\ (paramName, _, _) -> paramName) functionParams))
                                , (0, topLevelVars)] -- top level vars are assumed to be initialized
       (blk, _) <- tcBlock functionBody retTy varInit
       when (retTy /= TupleTy [] && not (hasTopLevelRetStmt (stmts functionBody))) $
         tcError $ [ "function " <> functionName <> " does not have top level return statements, but has return type "
                      <> show retTy ]
       when (functionName == "main" && (functionParams /= [] || retTy /= TupleTy [])) $
         tcError $ [ "function main should not have parameters or non () return type" ]
       when (blockTy blk /= Just (TupleTy [])) $
         tcError $ [ "unexpected function block type " <> fromMaybe "" (fmap show (blockTy blk)) <> " at " <> show functionLoc
                   , "top level function blocks should have type " <> show (TupleTy []) ]
       tcFunctionParamUnique functionParams
       return f { functionBody = blk }


tcFunctions :: [Function SourcePos] -> TC [Function SourcePos]
tcFunctions xs = do
  -- check top level function name uniqueness & insert into top level name map
  mapM_ (\f -> insertFunctionName (functionName f) (functionLoc f)) xs
  -- insert function sigs
  let funSigMap = M.fromList $ map (\f -> (functionName f, (functionParams f, retType f))) xs
      retType f = case functionRetType f of
                    Just ty -> ty
                    Nothing -> TupleTy []
  modify (\s -> s { tcFunctionSigMap = funSigMap })
  TCState{tcInputVarSet, tcDeclaredLinkConstantsMap} <- get
  let topLevelVars = S.union tcInputVarSet (S.fromList $ M.keys tcDeclaredLinkConstantsMap)
  -- check function body
  sequence $ map (tcFunctionBody topLevelVars) xs  

-- this is used when inserting new top level names to make sure that 
tcCheckTopLevelNames :: String -> SourcePos -> String -> TC ()
tcCheckTopLevelNames s pos varDesc = do
  m <- gets tcTopLevelNamesMap
  case M.lookup s m of
    Just (varDesc2, pos2) -> tcError $ [ varDesc <> " " <> s <> " (at " <> show pos <> ") "
                                       <> "conflicts with the name of an existing " <> varDesc2
                                       <> " (" <> fromMaybe "no source loc" (fmap show pos2) <> ")" ]
    Nothing -> return ()


-- modify the program to use tcUsedNamesMap

insertTopLevelName :: String -> Maybe SourcePos -> String -> Bool -> TC ()
insertTopLevelName name pos varDesc isReserved = do
  modify (\s -> s { tcTopLevelNamesMap = M.insert name (varDesc, pos) (tcTopLevelNamesMap s) })
  when isReserved $ do
    modify (\s -> s { tcTopLevelReservedNamesMap = M.insert name (varDesc, pos) (tcTopLevelReservedNamesMap s) })

insertDeclaredLinkConstant :: (SourcePos, String) -> TC ()
insertDeclaredLinkConstant (pos, name) = do
  tcCheckTopLevelNames name pos "declared link constant"
  modify (\s -> s { tcDeclaredLinkConstantsMap = M.insert name pos (tcDeclaredLinkConstantsMap s)
                  , tcVarMap = M.insert name (VarTy, "declared link constant", pos, 0) (tcVarMap s) })
  insertTopLevelName name (Just pos) "declared link constant" True

insertBuiltinFunction :: BuiltinFunction -> TC ()
insertBuiltinFunction f = do
  modify (\s -> s { tcBuiltinFunctionMap = M.insert (builtinFunctionName f) f (tcBuiltinFunctionMap s)})
  insertTopLevelName (builtinFunctionName f) Nothing "builtin function" True

tcProgram :: Program SourcePos -> TC (Program SourcePos)
tcProgram prog@(Program { functions 
                        , inputVars 
                        , declaredLinkConstants
                        }) = do
  mapM_ insertBuiltinFunction builtinFunctions
  mapM_ insertDeclaredLinkConstant declaredLinkConstants
  tcInputVars inputVars
  tcF <- tcFunctions functions
  return prog { functions = tcF }

{-# LANGUAGE NamedFieldPuns #-}
module MLogic.HighLevel.CallGraph (generateCallGraph, hasRecursiveCalls) where

import Control.Monad.State

import MLogic.HighLevel.Types

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

data CallGraphState = CallGraphState { cgBuiltinFunc :: Map String BuiltinFunction
                                     , currentFuncName :: String
                                     , cgCallGraph :: Map String (Set String)
                                     }

type CallGraph = State CallGraphState
-- a call graph is a map from function name F -> the set of functions called by F

-- ignores builtin functions

hasRecursiveCallsAux :: Map String (Set String) -> String -> Bool
hasRecursiveCallsAux cg funName =
    go (S.singleton funName) (outNode funName)
  where
    outNode f = case M.lookup f cg of
                  Just s -> s
                  Nothing -> S.empty
    go :: Set String -> Set String -> Bool
    go visited outNodes =
                 case S.intersection outNodes visited /= S.empty of
                   True -> True
                   False -> case outNodes == S.empty of
                              True -> False
                              False -> let newOutNodes = S.unions $ map outNode $ S.toList outNodes
                                       in go (S.union visited outNodes) newOutNodes 
hasRecursiveCalls :: Map String (Set String) -> Bool
hasRecursiveCalls cg = any (== True) $ map (hasRecursiveCallsAux cg) (M.keys cg)
  

generateCallGraph :: Map String BuiltinFunction -> [Function loc] -> Map String (Set String)
generateCallGraph builtinFuncs functions = cgCallGraph $ execState (mapM_ genFuncFunCall functions)
                                                 (CallGraphState { cgBuiltinFunc = builtinFuncs
                                                                 , currentFuncName = ""
                                                                 , cgCallGraph = M.empty })
 

genFunCallArgFunCall :: FunCallArg log -> CallGraph ()
genFunCallArgFunCall (ExprArg _ expr) = genExprFunCall expr
genFunCallArgFunCall _ = return ()


genExprFunCall :: Expr loc -> CallGraph ()
genExprFunCall (FunCallExpr _ _ funCallName args) = do
  builtinFuncs <- gets cgBuiltinFunc
  case M.lookup funCallName builtinFuncs of
    Nothing -> do
      callingFuncName <- gets currentFuncName
      modify (\s -> s { cgCallGraph = (M.insertWith S.union callingFuncName
                                               (S.singleton funCallName) (cgCallGraph s)) })
      mapM_ genFunCallArgFunCall args
    Just _ -> return ()
genExprFunCall (Add _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (Sub _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (Mul _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (Div _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (Mod _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (Pow _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (Equal _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (NotEqual _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (LAnd _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (LessThan _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (LessThanEq _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (GreaterThan _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (GreaterThanEq _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (StrictEqual _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (StrictNEq _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (Shl _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (Shr _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (LOr _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (BOr _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (BAnd _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (Xor _ _ expr1 expr2) = do
  genExprFunCall expr1
  genExprFunCall expr2
genExprFunCall (Flip _ _ expr1) = do
  genExprFunCall expr1
genExprFunCall (Not _ _ expr1) = do
  genExprFunCall expr1
genExprFunCall (Negate _ _ expr1) = do
  genExprFunCall expr1
genExprFunCall (Tuple _ _ exprs) = mapM_ genExprFunCall exprs
genExprFunCall (Lit _ _ _) = return ()
genExprFunCall (Var _ _ _) = return ()

genStmtFunCall :: Statement loc -> CallGraph ()
genStmtFunCall (DeclareStmt _ _ _ _ rhs) = mapM_ genStmtFunCall rhs
genStmtFunCall (AssignStmt _ _ _ rhs) = genStmtFunCall rhs
genStmtFunCall (BlockStmt _ _ blk) = genBlockFunCall blk
genStmtFunCall (ExprStmt _ _ expr) = genExprFunCall expr
genStmtFunCall (EndStmt _ _) = return ()
genStmtFunCall (ForStmt _ _ init cond step blk) = do
  genStmtFunCall init
  genExprFunCall cond
  genStmtFunCall step
  genBlockFunCall blk
genStmtFunCall (IfThenStmt _ _ cond blk) = do
  genExprFunCall cond
  genBlockFunCall blk
genStmtFunCall (IfThenElseStmt _ _ cond cons alt) = do
  genExprFunCall cond
  genBlockFunCall cons
  genBlockFunCall alt
genStmtFunCall (WhileStmt _ _ cond blk) = do
  genExprFunCall cond
  genBlockFunCall blk
genStmtFunCall (BreakStmt _ _) = return ()
genStmtFunCall (ReturnStmt _ _ rhs) = mapM_ genStmtFunCall rhs

genBlockFunCall :: Block loc -> CallGraph ()
genBlockFunCall (Block{stmts}) = mapM_ genStmtFunCall stmts

genFuncFunCall :: Function loc -> CallGraph ()
genFuncFunCall f = do
  s <- get
  put (s { currentFuncName = functionName f })
  genBlockFunCall $ functionBody f
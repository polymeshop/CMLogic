{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module MLogic.HighLevel.Types where

import Data.List

import MLogic.Assembly.Op

data Types = VarTy | TupleTy [Types] deriving (Eq)

instance Show Types where
  show VarTy = "Var"
  show (TupleTy tys) = "(" <> intercalate ", " (map show tys) <> ")"

data FunCallArg loc = ExprArg loc (Expr loc) | TargetArg loc String | SortArg loc String | BuildingGroupArg loc String deriving Show

data Expr loc where
  FunCallExpr :: loc -> Maybe Types -> String -> [FunCallArg loc] -> Expr loc
  
  Add :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  Sub :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  Mul :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  Div :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  Mod :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  Pow :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  Equal :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  NotEqual :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  LAnd :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  LessThan :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  LessThanEq :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  GreaterThan :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  GreaterThanEq :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  StrictEqual :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  StrictNEq :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  Shl :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  Shr :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  LOr :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  BOr :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  BAnd :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  Xor :: loc -> Maybe Types -> Expr loc -> Expr loc -> Expr loc
  Flip :: loc -> Maybe Types -> Expr loc -> Expr loc
  Not :: loc -> Maybe Types -> Expr loc -> Expr loc
  Negate :: loc -> Maybe Types -> Expr loc -> Expr loc
  Tuple :: loc -> Maybe Types -> [Expr loc] -> Expr loc
  Lit :: loc -> Maybe Types -> CL -> Expr loc
  Var :: loc -> Maybe Types -> String -> Expr loc
  deriving Show

type Statements loc = [Statement loc]

data Statement loc where
  DeclareStmt :: loc -> Maybe Types -> Pat loc -> Maybe Types -> Maybe (Statement loc) -> Statement loc
  AssignStmt :: loc -> Maybe Types -> Pat loc -> Statement loc -> Statement loc
  BlockStmt :: loc -> Maybe Types -> Block loc -> Statement loc
  ExprStmt :: loc -> Maybe Types -> Expr loc -> Statement loc
  EndStmt :: loc -> Maybe Types -> Statement loc
  ForStmt :: loc -> Maybe Types -> Statement loc -> Expr loc -> Statement loc -> Block loc -> Statement loc 
  IfThenStmt :: loc -> Maybe Types -> Expr loc -> Block loc -> Statement loc
  IfThenElseStmt :: loc -> Maybe Types -> Expr loc -> Block loc -> Block loc -> Statement loc
  WhileStmt :: loc -> Maybe Types -> Expr loc -> Block loc -> Statement loc
  BreakStmt :: loc -> Maybe Types -> Statement loc
  ReturnStmt :: loc -> Maybe Types -> Maybe (Statement loc) -> Statement loc
  deriving Show

class HasPos a loc | a -> loc where
  getPos :: a -> loc

instance HasPos (FunCallArg loc) loc where
  getPos (ExprArg pos _) = pos
  getPos (TargetArg pos _) = pos
  getPos (SortArg pos _) = pos
  getPos (BuildingGroupArg pos _) = pos

instance HasPos (Expr loc) loc where
  getPos (FunCallExpr pos _ _ _) = pos
  getPos (Add pos _ _ _) = pos
  getPos (Sub pos _ _ _) = pos
  getPos (Mul pos _ _ _) = pos
  getPos (Div pos _ _ _) = pos
  getPos (Mod pos _ _ _) = pos
  getPos (Pow pos _ _ _) = pos
  getPos (Equal pos _ _ _) = pos
  getPos (NotEqual pos _ _ _) = pos
  getPos (LAnd pos _ _ _) = pos
  getPos (LessThan pos _ _ _) = pos
  getPos (LessThanEq pos _ _ _) = pos
  getPos (GreaterThan pos _ _ _) = pos
  getPos (GreaterThanEq pos _ _ _) = pos
  getPos (StrictEqual pos _ _ _) = pos
  getPos (StrictNEq pos _ _ _) = pos
  getPos (Shl pos _ _ _) = pos
  getPos (Shr pos _ _ _) = pos
  getPos (LOr pos _ _ _) = pos
  getPos (BOr pos _ _ _) = pos
  getPos (BAnd pos _ _ _) = pos
  getPos (Xor pos _ _ _) = pos
  getPos (Flip pos _ _) = pos
  getPos (Not pos _ _) = pos
  getPos (Negate pos _ _) = pos
  getPos (Tuple pos _ _) = pos
  getPos (Lit pos _ _) = pos
  getPos (Var pos _ _) = pos

class HasType a where
  getType :: a -> Maybe Types

instance HasType (Expr loc) where
  getType (FunCallExpr _ ty _ _) = ty
  getType (Add _ ty _ _) = ty
  getType (Sub _ ty _ _) = ty
  getType (Mul _ ty _ _) = ty
  getType (Div _ ty _ _) = ty
  getType (Mod _ ty _ _) = ty
  getType (Pow _ ty _ _) = ty
  getType (Equal _ ty _ _) = ty
  getType (NotEqual _ ty _ _) = ty
  getType (LAnd _ ty _ _) = ty
  getType (LessThan _ ty _ _) = ty
  getType (LessThanEq _ ty _ _) = ty
  getType (GreaterThan _ ty _ _) = ty
  getType (GreaterThanEq _ ty _ _) = ty
  getType (StrictEqual _ ty _ _) = ty
  getType (StrictNEq _ ty _ _) = ty
  getType (Shl _ ty _ _) = ty
  getType (Shr _ ty _ _) = ty
  getType (LOr _ ty _ _) = ty
  getType (BOr _ ty _ _) = ty
  getType (BAnd _ ty _ _) = ty
  getType (Xor _ ty _ _) = ty
  getType (Flip _ ty _) = ty
  getType (Not _ ty _) = ty
  getType (Negate _ ty _) = ty
  getType (Tuple _ ty _) = ty
  getType (Lit _ ty _) = ty
  getType (Var _ ty _) = ty


instance HasType (Statement loc) where
  getType (DeclareStmt _ ty _ _ _)  = ty
  getType (AssignStmt _ ty _ _) = ty
  getType (BlockStmt _ ty _) = ty
  getType (ExprStmt _ ty _) = ty
  getType (EndStmt _ ty) = ty
  getType (ForStmt _ ty _ _ _ _) = ty
  getType (IfThenStmt _ ty _ _) = ty
  getType (IfThenElseStmt _ ty _ _ _) = ty
  getType (WhileStmt _ ty _ _) = ty
  getType (BreakStmt _ ty) = ty
  getType (ReturnStmt _ ty _) = ty

data Block loc = Block { stmts :: Statements loc, blockTy :: Maybe Types  } deriving Show

data CL =
        AtConstant String
      | DoubleLit Double
      | StrLit String
      | TupleLit [CL]
      | TrueL
      | FalseL
      | Null

instance Show CL where
  show (AtConstant str) = str
  show (DoubleLit d) = show d
  show (StrLit str) = show str
  show (TupleLit lits) = "(" <> intercalate ", " (map show lits) <> ")"
  show TrueL = "true"
  show FalseL = "false"
  show Null = "null"

data Pat loc = VarPat loc String
             | IgnorePat loc
             | TuplePat loc [Pat loc]

getPatVars :: Pat a -> [String]
getPatVars (VarPat _ s) = [s]
getPatVars (IgnorePat _) = []
getPatVars (TuplePat _ pats) = concat $ map getPatVars pats

instance Show (Pat a) where
  show (VarPat _ str) = str
  show (IgnorePat _) = "_"
  show (TuplePat _ pats) = "(" <> intercalate ", " (map show pats) <> ")"

data Function loc
  = Function { functionName :: String
             , functionParams :: [(String, loc, Types)]
             , functionRetType :: Maybe Types
             , functionBody :: Block loc
             , functionLoc :: loc
             }
     deriving Show

data BuiltinTy = BuiltinVar | BuiltinTarget | BuiltinSort | BuiltinBuildingGroup deriving (Show, Eq)

data BuiltinFunction = BuiltinFunction { builtinFunctionName :: String
                                       , builtinFunctionParams :: [(String, BuiltinTy)]
                                       , builtinFunctionRetType :: Types
                                       } deriving Eq

data Program loc
  = Program { functions :: [Function loc]
            , inputVars :: [(loc, String, Maybe CL)]
            , declaredLinkConstants :: [(loc, String)] }
       deriving Show
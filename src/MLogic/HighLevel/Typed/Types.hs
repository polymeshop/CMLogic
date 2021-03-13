{-# LANGUAGE ExistentialQuantification, GADTs #-}
module MLogic.HighLevel.Typed.Types where

import MLogic.Assembly.Op

import Data.Endo

data Obj
newtype Numeric = Numeric Double

data TExpr a where
  -- impure
  TERead :: TExpr Obj -> TConstant a -> TExpr a
  TEGetlink :: TExpr Obj -> TExpr Numeric -> TExpr Obj
  TERadar :: TExpr Obj -> Target -> Target -> Target -> TExpr Numeric -> SortBy -> TExpr Obj
  TESensor :: TExpr Obj -> TExpr (Property a) -> TExpr a
  TEURadar :: Target -> Target -> Target -> TExpr Numeric -> SortBy -> TExpr Obj
  TEULocateOre :: TExpr Obj -> TExpr (Numeric, Numeric, Bool)
  TEULocateBuilding :: ULocateBuildingType -> TExpr Bool -> TExpr (Numeric, Numeric, Bool, Obj)
  TEULocateSpawn :: TExpr (Numeric, Numeric, Bool, Obj)
  TEULocateDamaged :: TExpr (Numeric, Numeric, Bool, Obj)
  --- pure (except Rand)
  TAdd :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TSub :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TMul :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TDiv :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TIdiv :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TMod :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TPow :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TEqual :: TExpr a -> TExpr a -> TExpr a
  TNotEqual :: TExpr a -> TExpr a -> TExpr a
  TLand :: TExpr Bool -> TExpr Bool -> TExpr Bool
  TLessThan :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TLessThanEq :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TGreaterThan :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TGreaterThanEq :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TStrictEqual :: TExpr a -> TExpr a -> TExpr a
  TStrictNEq :: TExpr a -> TExpr a -> TExpr a
  TShl :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TShr :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TLOr :: TExpr Bool -> TExpr Bool -> TExpr Bool
  TBOr :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TBAnd :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TXor :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TFlip :: TExpr Numeric -> TExpr Numeric
  TMax :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TMin :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TAtan2 :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TDst :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TNoise :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric
  TNot :: TExpr Bool -> TExpr Bool
  TAbs :: TExpr Numeric -> TExpr Numeric
  TLog :: TExpr Numeric -> TExpr Numeric
  TLog10 :: TExpr Numeric -> TExpr Numeric
  TSin :: TExpr Numeric -> TExpr Numeric
  TCos :: TExpr Numeric -> TExpr Numeric
  Tan :: TExpr Numeric -> TExpr Numeric
  TFloor :: TExpr Numeric -> TExpr Numeric
  TCeil :: TExpr Numeric -> TExpr Numeric
  TSqrt :: TExpr Numeric -> TExpr Numeric
  TRand :: TExpr Numeric -> TExpr Numeric
  TLit :: TLit a -> TExpr a
  -- type cast
  Cast :: TExpr a -> TExpr b


data TControl = TEnabled (TExpr Obj) (TExpr Bool)
              | TShoot (TExpr Obj) (TExpr Numeric) (TExpr Numeric) (TExpr Bool)
              | TShootp (TExpr Obj) (TExpr Obj) (TExpr Bool)
              | TConfigure (TExpr Obj) (TExpr Obj)

data TUControl a where
  TUStop :: TUControl ()
  TUMove :: TExpr Numeric -> TExpr Numeric -> TUControl ()
  TUApproach :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric -> TUControl ()
  TUBoost :: TExpr Bool -> TUControl ()
  TUPathfind :: TUControl ()
  TUTarget :: TExpr Numeric -> TExpr Numeric -> TExpr Bool -> TUControl ()
  TUTargetp :: TExpr Obj -> TExpr Bool -> TUControl ()
  TUItemDrop :: TExpr Obj -> TExpr Numeric -> TUControl ()
  TUItemTake :: TExpr Obj -> TExpr Obj -> TExpr Numeric -> TUControl ()
  TUPayDrop :: TUControl ()
  TUPayTake :: TExpr Numeric -> TUControl ()
  TUMine :: TExpr Numeric -> TExpr Numeric -> TUControl ()
  TUFlag :: TExpr Numeric -> TUControl ()
  TUBuild :: TExpr Numeric -> TExpr Numeric -> TExpr Obj -> TExpr Numeric -> TExpr Obj -> TUControl ()
  TUGetBlock :: TExpr Numeric -> TExpr Numeric -> TUControl (TVar Obj, TVar Obj)
  TUWithin :: TExpr Numeric -> TExpr Numeric -> TExpr Numeric -> TUControl (TVar Bool)


data TStatements = TStatements [FTStatement]

data FTStatement = forall a. FTStatement (TStatement a)



data TStatement a where
  TDeclare :: TVar a -> Maybe (TExpr a) -> TStatement ()
  TAssign :: TVar a -> TExpr a -> TStatement ()
  TWriteStmt :: TExpr a -> TExpr Obj -> TConstant a -> TStatement ()
  TBlockStmt :: TBlock -> TStatement ()
-- TDrawStmt  not supported yet
  TPrintStmt :: TExpr a -> TStatement ()
  TPrintFlushStmt :: TExpr Obj -> TStatement ()
  TControlStmt :: TControl -> TStatement ()
  TEndStmt :: TStatement ()
  TUBindStmt :: TExpr Obj -> TStatement ()
  TUControlStmt :: TUControl a -> TStatement a
  TFunCallStmt :: String -> [TExpr a] -> TStatement ()
  TForStmt :: TStatement () -> TExpr Bool -> TStatement ()
  TIfThenStmt :: TExpr Bool -> TStatements -> TStatement ()
  TIfThenElseStmt :: TExpr Bool -> TStatements -> TStatements -> TStatement ()
  TWhileStmt :: TExpr Bool -> TStatements -> TStatement ()
  

data TBlock = TBlock { stmts :: [FTStatement] }



data TVar a where
  ObjTVar :: String -> TVar Obj
  BoolTVar :: String -> TVar Bool
  NumTVar :: String -> TVar Numeric
  StrTVar :: String -> TVar String
  PropTVar :: String -> TVar (Property a)

data Property a

data TConstant a where
  ObjC :: String -> TConstant Obj
  BoolC :: String -> TConstant Bool
  NumC :: String -> TConstant Numeric
  StrC :: String -> TConstant String
  PropC :: String -> TConstant (Property a)

-- constant or lit (CL)
data CL a =
    C (TConstant a)
  | L a
  | Null

data TLit a where
  TObjLit :: CL Obj -> TLit Obj
  TBoolLit :: CL Bool -> TLit Bool
  TNumLit :: CL Numeric  -> TLit Numeric
  TStrLit :: CL String -> TLit String
  TPropLit :: CL (Property a) -> TLit (Property a)

data FTVar = forall a. FTVar (TVar a)
type FunctionName = String
type TArg = FTVar
data TMainArg = forall a. TMainArg (TVar a, Maybe (TLit a))

data TypedProgram = TypedProgram
      { inputTVars :: [TMainArg]
      , main :: TBlock
      , functions :: [(String, [TArg], TBlock)]
      }


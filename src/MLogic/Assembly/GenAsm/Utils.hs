{-# LANGUAGE RebindableSyntax, NamedFieldPuns #-}
module MLogic.Assembly.GenAsm.Utils where

import MLogic.Assembly.GenAsm.Types
import MLogic.Assembly.Op
import MLogic.Assembly.Operand
import MLogic.Constants


import Control.Monad.State.Class


import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

import Prelude

import Data.Endo

class IsInstr a where
  fromInstr :: Instr -> a
  toInstr :: a -> Maybe Instr

instance IsInstr Instr where
  fromInstr = id
  toInstr i = Just i

addInstr :: (IsInstr i, Monad m) => Instr -> GenAsmM i m ()
addInstr i = do
  s <- get
  put (s { genAsmInstrs = composeEndo (genAsmInstrs s) (Endo ([fromInstr i]++)) })

newLab :: (IsInstr i, Monad m) => GenAsmM i m String
newLab = do
  lv <- gets genAsmLabVar
  s <- get
  let labStr = "lab" ++ show lv
  put (s { genAsmLabVar = lv + 1 })
  return labStr

addLab :: (IsInstr i, Monad m) => String -> GenAsmM i m ()
addLab s = addInstr (LabelI s)

addNewLab :: (IsInstr i, Monad m) => GenAsmM i m String
addNewLab = do
  l <- newLab
  addLab l
  return l

newVarWithHint :: (IsInstr i, Monad m) => String -> GenAsmM i m String
newVarWithHint hint = do
  GenAsmState {genAsmVarCounter, genAsmVars} <- get
  varNum <-
    case M.lookup hint genAsmVarCounter of
      Just num -> return num
      Nothing -> do
        modify $ \s -> s { genAsmVarCounter = M.insert hint 1 genAsmVarCounter }
        return 0
  let genName = hint <> show varNum
  case S.member genName genAsmVars of
    True -> do
      modify $ \s -> s { genAsmVarCounter = M.insert hint (varNum + 1) genAsmVarCounter }
      newVarWithHint hint
    False -> do
      modify $ \s -> s { genAsmVars = S.insert genName genAsmVars }
      return genName

newVar :: (IsInstr i, Monad m) => GenAsmM i m String
newVar = newVarWithHint "_v"

newVarInit :: (IsInstr i, Monad m) => Operand -> GenAsmM i m String
newVarInit op = do
  v <- newVar
  set (var v) op
  return v


str :: String -> Operand
str = StringOperand

var :: String -> Operand
var = VarOperand

double :: Double -> Operand
double = DoubleOperand




-- set variable to some value
set :: (IsInstr i, Monad m) => Operand -> Operand -> GenAsmM i m ()
set op v = addInstr $ SetI op v

uMove :: (IsInstr i, Monad m) => (Operand, Operand) -> GenAsmM i m ()
uMove (x, y) = addInstr $ UControlI (UMove x y)

uApproach :: (IsInstr i, Monad m) => (Operand, Operand) -> Operand -> GenAsmM i m ()
uApproach (x, y) radius = addInstr $ UControlI (UApproach x y radius)

uBoost :: (IsInstr i, Monad m) => Operand -> GenAsmM i m ()
uBoost enabled = addInstr $ UControlI (UBoost enabled)

uPathFind :: (IsInstr i, Monad m) => GenAsmM i m ()
uPathFind = addInstr $ UControlI UPathfind

uTarget :: (IsInstr i, Monad m) => (Operand, Operand) -> Operand -> GenAsmM i m ()
uTarget (x, y) shoot = addInstr $ UControlI (UTarget x y shoot)

uTargetp :: (IsInstr i, Monad m) => Operand -> Operand -> GenAsmM i m ()
uTargetp unit shoot = addInstr $ UControlI (UTargetp unit shoot)

uItemDrop :: (IsInstr i, Monad m) => Operand -> Operand -> GenAsmM i m ()
uItemDrop to amount = addInstr $ UControlI (UItemDrop to amount)

uItemTake :: (IsInstr i, Monad m) => Operand -> Operand -> Operand -> GenAsmM i m ()
uItemTake from ty amount = addInstr $ UControlI (UItemTake from ty amount)

uPayDrop :: (IsInstr i, Monad m) => GenAsmM i m ()
uPayDrop = addInstr $ UControlI UPayDrop

uPayTake :: (IsInstr i, Monad m) => Operand -> GenAsmM i m ()
uPayTake takeUnits = addInstr $ UControlI (UPayTake takeUnits)

uMine :: (IsInstr i, Monad m) => (Operand, Operand) -> GenAsmM i m ()
uMine (x, y) = addInstr $ UControlI (UMine x y)

uFlag :: (IsInstr i, Monad m) => Operand -> GenAsmM i m ()
uFlag flag = addInstr $ UControlI (UFlag flag)

uBuild :: (IsInstr i, Monad m) => (Operand, Operand) -> Operand -> Operand -> Operand -> GenAsmM i m ()
uBuild (x, y) block rotation config = addInstr $ UControlI (UBuild x y block rotation config)

uGetBlock :: (IsInstr i, Monad m) => (Operand, Operand) -> (Operand, Operand) -> GenAsmM i m ()
uGetBlock (ty, building) (x, y) = addInstr $ UControlI (UGetBlock x y ty building)

uGetBlock' :: (IsInstr i, Monad m) => (Operand, Operand) -> GenAsmM i m (Operand, Operand)
uGetBlock' (x, y) = do
  ty <- newVar
  building <- newVar
  uGetBlock (var ty, var building) (x, y)
  return (var ty, var building)

uWithin :: (IsInstr i, Monad m) => Operand -> (Operand, Operand) -> Operand -> GenAsmM i m ()
uWithin result (x, y) radius = addInstr $ UControlI (UWithin x y radius result)

uWithin' :: (IsInstr i, Monad m) => (Operand, Operand) -> Operand -> GenAsmM i m Operand
uWithin' (x, y) radius = do
  result <- newVar
  uWithin (var result) (x, y) radius
  return (var result)

uLocateOre :: (IsInstr i, Monad m) => ((Operand, Operand), Operand) -> Operand -> GenAsmM i m ()
uLocateOre ((oreX, oreY), found) oreType =
     addInstr $ ULocateI (ULocateOre oreType oreX oreY found)

uLocateOre' :: (IsInstr i, Monad m) => Operand -> GenAsmM i m ((Operand, Operand), Operand)
uLocateOre' oreType = do
  oreX <- newVar
  oreY <- newVar
  found <- newVar
  uLocateOre ((var oreX, var oreY), var found) oreType
  return ((var oreX, var oreY), var found)

uLocateBuilding :: (IsInstr i, Monad m) => ((Operand, Operand), Operand, Operand) -> Operand -> Operand -> GenAsmM i m ()
uLocateBuilding ((outX, outY), found, building) ty enemy =
          addInstr $ ULocateI (ULocateBuilding ty enemy outX outY found building)

uLocateBuilding' :: (IsInstr i, Monad m) => Operand -> Operand -> GenAsmM i m ((Operand, Operand), Operand, Operand)
uLocateBuilding' ty enemy = do
          outX <- newVar
          outY <- newVar
          found <- newVar
          building <- newVar
          uLocateBuilding ((var outX, var outY), var found, var building) ty enemy
          return ((var outX, var outY), var found, var building)

uLocateAlliedCore :: (IsInstr i, Monad m) => ((Operand, Operand), Operand, Operand) -> GenAsmM i m ()
uLocateAlliedCore ((outX, outY), found, building) =
    uLocateBuilding ((outX, outY), found, building) (var "core") falseC

uLocateAlliedCore' :: (IsInstr i, Monad m) => GenAsmM i m ((Operand, Operand), Operand, Operand)
uLocateAlliedCore' = uLocateBuilding' (var "core") falseC 

uLocateEnemyCore :: (IsInstr i, Monad m) => ((Operand, Operand), Operand, Operand) -> GenAsmM i m ()
uLocateEnemyCore ((outX, outY), found, building) = 
    uLocateBuilding ((outX, outY), found, building) (var "core") trueC

uLocateEnemyCore' :: (IsInstr i, Monad m) => GenAsmM i m ((Operand, Operand), Operand, Operand)
uLocateEnemyCore' = uLocateBuilding' (var "core") trueC


uLocateSpawn :: (IsInstr i, Monad m) => ((Operand, Operand), Operand, Operand) -> GenAsmM i m ()
uLocateSpawn ((outX, outY), found, building) = do
  addInstr $ ULocateI (ULocateSpawn outX outY found building)

uLocateSpawn' :: (IsInstr i, Monad m) => GenAsmM i m ((Operand, Operand), Operand, Operand)
uLocateSpawn' = do
  outX <- newVar
  outY <- newVar
  found <- newVar
  building <- newVar
  addInstr $ ULocateI (ULocateSpawn (var outX) (var outY) (var found) (var building))
  return ((var outX, var outY), var found, var building)

uLocateDamaged :: (IsInstr i, Monad m) => ((Operand, Operand), Operand, Operand) -> GenAsmM i m ()
uLocateDamaged ((outX, outY), found, building) = do
  addInstr $ ULocateI (ULocateDamaged outX outY found building)

uLocateDamaged' :: (IsInstr i, Monad m) => GenAsmM i m ((Operand, Operand), Operand, Operand)
uLocateDamaged' = do
  outX <- newVar
  outY <- newVar
  found <- newVar
  building <- newVar
  addInstr $ ULocateI (ULocateDamaged (var outX) (var outY) (var found) (var building))
  return ((var outX, var outY), var found, var building)


bindUnit :: (IsInstr i, Monad m) => Operand -> GenAsmM i m ()
bindUnit s = addInstr $ UBindI s

jumpLabel :: (IsInstr i, Monad m) => String -> GenAsmM i m ()
jumpLabel s = addInstr $ JumpI (LabTarget s) JAlways

jumpLabelEq :: (IsInstr i, Monad m) => String -> Operand -> Operand -> GenAsmM i m ()
jumpLabelEq s a b = addInstr $ JumpI (LabTarget s) (JEq a b)

jumpLabelNeq :: (IsInstr i, Monad m) => String -> Operand -> Operand -> GenAsmM i m ()
jumpLabelNeq s a b = addInstr $ JumpI (LabTarget s) (JNeq a b)

jumpLabelSEq :: (IsInstr i, Monad m) => String -> Operand -> Operand -> GenAsmM i m ()
jumpLabelSEq s a b = addInstr $ JumpI (LabTarget s) (JSEq a b)


jumpLabelLt :: (IsInstr i, Monad m) => String -> Operand -> Operand -> GenAsmM i m ()
jumpLabelLt s a b = addInstr $ JumpI (LabTarget s) (JLt a b)

jumpLabelLe :: (IsInstr i, Monad m) => String -> Operand -> Operand -> GenAsmM i m ()
jumpLabelLe s a b = addInstr $ JumpI (LabTarget s) (JLe a b)

jumpLabelGt :: (IsInstr i, Monad m) => String -> Operand -> Operand -> GenAsmM i m ()
jumpLabelGt s a b = addInstr $ JumpI (LabTarget s) (JGt a b)

jumpLabelGe :: (IsInstr i, Monad m) => String -> Operand -> Operand -> GenAsmM i m ()
jumpLabelGe s a b = addInstr $ JumpI (LabTarget s) (JGe a b)

printFlush :: (IsInstr i, Monad m) => Operand -> GenAsmM i m ()
printFlush h = addInstr $ PrintFlushI h

print :: (IsInstr i, Monad m, ToOperand a) => a -> GenAsmM i m ()
print op = addInstr $ PrintI (toOperand op)

printLn :: (IsInstr i, Monad m, ToOperand a) => a -> GenAsmM i m ()
printLn op = do
  addInstr $ PrintI (toOperand op)
  addInstr $ PrintI (toOperand (StringOperand "\n"))


getLink :: (IsInstr i, Monad m) => Operand -> Operand -> GenAsmM i m ()
getLink dst lnk = addInstr $ GetlinkI dst lnk

getLink' :: (IsInstr i, Monad m) => Operand -> GenAsmM i m Operand
getLink' lnk = do
  v <- newVar
  getLink (var v) lnk
  return (var v)

sense :: (IsInstr i, Monad m) => Operand -> Operand -> Operand -> GenAsmM i m ()
sense result unit prop = addInstr $ SensorI result unit prop

sense' :: (IsInstr i, Monad m) => Operand -> Operand -> GenAsmM i m Operand
sense' unit prop = do
  v <- newVar
  addInstr $ SensorI (var v) unit prop
  return (var v)

getPos :: (IsInstr i, Monad m) => (Operand, Operand) -> Operand -> GenAsmM i m ()
getPos (outX, outY) obj = do
  sense outX obj xC
  sense outY obj yC

getPos' :: (IsInstr i, Monad m) => Operand -> GenAsmM i m (Operand, Operand)
getPos' obj = do
  x <- newVar
  y <- newVar
  getPos (var x, var y) obj
  return (var x, var y)

getShootPos :: (IsInstr i, Monad m) => (Operand, Operand) -> Operand -> GenAsmM i m ()
getShootPos (outX, outY) obj = do
  sense outX obj shootXC
  sense outY obj shootYC

getShootPos' :: (IsInstr i, Monad m) => Operand -> GenAsmM i m (Operand, Operand)
getShootPos' obj = do
  x <- newVar
  y <- newVar
  getShootPos (var x, var y) obj
  return (var x, var y)


getUnitFlag :: (IsInstr i, Monad m) => Operand -> GenAsmM i m ()
getUnitFlag flag = sense flag unitC (var "@flag")

getUnitFlag' :: (IsInstr i, Monad m) => GenAsmM i m Operand
getUnitFlag' = sense' unitC (var "@flag")

setUnitFlag :: (IsInstr i, Monad m) => Operand -> GenAsmM i m ()
setUnitFlag op = addInstr $ UControlI (UFlag op)

infloop :: GenAsm ()
infloop = do
  self <- addNewLab
  jumpLabel self

loopProg :: GenAsm () -> GenAsm ()
loopProg p = do
  l <- addNewLab
  p >> jumpLabel l


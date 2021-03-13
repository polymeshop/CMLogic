module MLogic.HighLevel.BuiltinFunctions where

import MLogic.HighLevel.Types

readFunction :: BuiltinFunction
readFunction =
  BuiltinFunction { builtinFunctionName = "read"
                  , builtinFunctionParams = [("cell", BuiltinVar), ("at", BuiltinVar)]
                  , builtinFunctionRetType = VarTy
                  }

writeFunction :: BuiltinFunction
writeFunction =
  BuiltinFunction { builtinFunctionName = "write"
                  , builtinFunctionParams = [("value", BuiltinVar), ("cell", BuiltinVar), ("at", BuiltinVar)]
                  , builtinFunctionRetType = TupleTy []
                  }

drawClearFunction :: BuiltinFunction
drawClearFunction =
  BuiltinFunction { builtinFunctionName = "drawClear"
                  , builtinFunctionParams = [ ("r", BuiltinVar), ("g", BuiltinVar), ("b", BuiltinVar)]
                  , builtinFunctionRetType = TupleTy []
                  }

drawColorFunction :: BuiltinFunction
drawColorFunction =
  BuiltinFunction { builtinFunctionName = "drawColor"
                  , builtinFunctionParams = [ ("r", BuiltinVar), ("g", BuiltinVar), ("b", BuiltinVar), ("a", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

drawStrokeFunction :: BuiltinFunction
drawStrokeFunction =
  BuiltinFunction { builtinFunctionName = "drawStroke"
                  , builtinFunctionParams = [ ("width", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

drawLineFunction :: BuiltinFunction
drawLineFunction =
  BuiltinFunction { builtinFunctionName = "drawLine"
                  , builtinFunctionParams = [ ("x", BuiltinVar), ("y", BuiltinVar), ("x2", BuiltinVar), ("y2", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

drawRectFunction :: BuiltinFunction
drawRectFunction =
  BuiltinFunction { builtinFunctionName = "drawRect"
                  , builtinFunctionParams = [ ("x", BuiltinVar), ("y", BuiltinVar), ("width", BuiltinVar), ("height", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

drawLineRectFunction :: BuiltinFunction
drawLineRectFunction =
  BuiltinFunction { builtinFunctionName = "drawLineRect"
                  , builtinFunctionParams = [ ("x", BuiltinVar), ("y", BuiltinVar), ("width", BuiltinVar), ("height", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

drawPolyFunction :: BuiltinFunction
drawPolyFunction =
  BuiltinFunction { builtinFunctionName = "drawPoly"
                  , builtinFunctionParams = [ ("x", BuiltinVar), ("y", BuiltinVar), ("sides", BuiltinVar), ("radius", BuiltinVar), ("rotation", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

drawLinePolyFunction :: BuiltinFunction
drawLinePolyFunction =
  BuiltinFunction { builtinFunctionName = "drawLinePoly"
                  , builtinFunctionParams = [ ("x", BuiltinVar), ("y", BuiltinVar), ("sides", BuiltinVar), ("radius", BuiltinVar), ("rotation", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

drawTriangleFunction :: BuiltinFunction
drawTriangleFunction =
  BuiltinFunction { builtinFunctionName = "drawTriangle"
                  , builtinFunctionParams = [ ("x", BuiltinVar), ("y", BuiltinVar), ("x2", BuiltinVar), ("y2", BuiltinVar), ("x3", BuiltinVar), ("y3", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

drawImageFunction :: BuiltinFunction
drawImageFunction =
  BuiltinFunction { builtinFunctionName = "drawImage"
                  , builtinFunctionParams = [ ("x", BuiltinVar), ("y", BuiltinVar), ("image", BuiltinVar), ("size", BuiltinVar), ("rotation", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }


printFunction :: BuiltinFunction
printFunction =
  BuiltinFunction { builtinFunctionName = "print"
                  , builtinFunctionParams = [("output", BuiltinVar)]
                  , builtinFunctionRetType = TupleTy []
                  }

drawFlushFunction :: BuiltinFunction
drawFlushFunction =
  BuiltinFunction { builtinFunctionName = "drawFlush"
                  , builtinFunctionParams = [ ("to", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

printFlushFunction :: BuiltinFunction
printFlushFunction =
  BuiltinFunction { builtinFunctionName = "printFlush"
                  , builtinFunctionParams = [("link", BuiltinVar)]
                  , builtinFunctionRetType = TupleTy []
                  }

getLinkFunction :: BuiltinFunction
getLinkFunction =
  BuiltinFunction { builtinFunctionName = "getLink"
                  , builtinFunctionParams = [("linkNo", BuiltinVar)]
                  , builtinFunctionRetType = VarTy
                  }


controlEnabledFunction :: BuiltinFunction
controlEnabledFunction =
  BuiltinFunction { builtinFunctionName = "controlEnabled"
                  , builtinFunctionParams = [ ("obj", BuiltinVar), ("value", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }


controlShootFunction :: BuiltinFunction
controlShootFunction =
  BuiltinFunction { builtinFunctionName = "controlShoot"
                  , builtinFunctionParams = [ ("obj", BuiltinVar), ("x", BuiltinVar), ("y", BuiltinVar), ("shoot", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

controlShootpFunction :: BuiltinFunction
controlShootpFunction =
  BuiltinFunction { builtinFunctionName = "controlShootp"
                  , builtinFunctionParams = [ ("obj", BuiltinVar), ("unit", BuiltinVar), ("shoot", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

controlConfigureFunction :: BuiltinFunction
controlConfigureFunction =
  BuiltinFunction { builtinFunctionName = "controlConfigure"
                  , builtinFunctionParams = [ ("obj", BuiltinVar), ("val", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }



radarFunction :: BuiltinFunction
radarFunction =
  BuiltinFunction { builtinFunctionName = "radar"
                  , builtinFunctionParams = [ ("from", BuiltinVar), ("target1", BuiltinTarget), ("target2", BuiltinTarget)
                                            , ("target3", BuiltinTarget), ("order", BuiltinVar), ("sort", BuiltinSort) ]
                  , builtinFunctionRetType = VarTy
                  }


sensorFunction :: BuiltinFunction
sensorFunction =
  BuiltinFunction { builtinFunctionName = "sensor"
                  , builtinFunctionParams = [ ("obj", BuiltinVar), ("property", BuiltinVar) ]
                  , builtinFunctionRetType = VarTy
                  }



ubindFunction :: BuiltinFunction
ubindFunction =
  BuiltinFunction { builtinFunctionName = "ubind"
                  , builtinFunctionParams = [("unitType", BuiltinVar)]
                  , builtinFunctionRetType = TupleTy []
                  }


ucontrolStopFunction :: BuiltinFunction
ucontrolStopFunction =
  BuiltinFunction { builtinFunctionName = "ucontrolStop"
                  , builtinFunctionParams = []
                  , builtinFunctionRetType = TupleTy []
                  }

ucontrolMoveFunction :: BuiltinFunction
ucontrolMoveFunction =
  BuiltinFunction { builtinFunctionName = "ucontrolMove"
                  , builtinFunctionParams = [ ("x", BuiltinVar), ("y", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

ucontrolApproachFunction :: BuiltinFunction
ucontrolApproachFunction =
  BuiltinFunction { builtinFunctionName = "ucontrolApproach"
                  , builtinFunctionParams = [ ("x", BuiltinVar), ("y", BuiltinVar), ("radius", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

ucontrolBoostFunction :: BuiltinFunction
ucontrolBoostFunction =
  BuiltinFunction { builtinFunctionName = "ucontrolBoost"
                  , builtinFunctionParams = [ ("enabled", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

ucontrolPathfindFunction :: BuiltinFunction
ucontrolPathfindFunction =
  BuiltinFunction { builtinFunctionName = "ucontrolPathfind"
                  , builtinFunctionParams = []
                  , builtinFunctionRetType = TupleTy []
                  }

ucontrolTargetFunction :: BuiltinFunction
ucontrolTargetFunction =
  BuiltinFunction { builtinFunctionName = "ucontrolTarget"
                  , builtinFunctionParams = [ ("x", BuiltinVar), ("y", BuiltinVar), ("shoot", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

ucontrolTargetpFunction :: BuiltinFunction
ucontrolTargetpFunction =
  BuiltinFunction { builtinFunctionName = "ucontrolTargetp"
                  , builtinFunctionParams = [ ("unit", BuiltinVar), ("shoot", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

ucontrolItemDropFunction :: BuiltinFunction
ucontrolItemDropFunction =
  BuiltinFunction { builtinFunctionName = "ucontrolItemDrop"
                  , builtinFunctionParams = [ ("to", BuiltinVar), ("amount", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

ucontrolItemTakeFunction :: BuiltinFunction
ucontrolItemTakeFunction =
  BuiltinFunction { builtinFunctionName = "ucontrolItemTake"
                  , builtinFunctionParams = [ ("from", BuiltinVar), ("item", BuiltinVar), ("amount", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

ucontrolPayDropFunction :: BuiltinFunction
ucontrolPayDropFunction =
  BuiltinFunction { builtinFunctionName = "ucontrolPayDrop"
                  , builtinFunctionParams = []
                  , builtinFunctionRetType = TupleTy []
                  }

ucontrolPayTakeFunction :: BuiltinFunction
ucontrolPayTakeFunction =
  BuiltinFunction { builtinFunctionName = "ucontrolPayTake"
                  , builtinFunctionParams = [ ("takeUnits", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

ucontrolMineFunction :: BuiltinFunction
ucontrolMineFunction =
  BuiltinFunction { builtinFunctionName = "ucontrolMine"
                  , builtinFunctionParams = [ ("x", BuiltinVar), ("y", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

ucontrolFlagFunction :: BuiltinFunction
ucontrolFlagFunction =
  BuiltinFunction { builtinFunctionName = "ucontrolFlag"
                  , builtinFunctionParams = [ ("value", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

ucontrolBuildFunction :: BuiltinFunction
ucontrolBuildFunction =
  BuiltinFunction { builtinFunctionName = "ucontrolBuild"
                  , builtinFunctionParams = [ ("x", BuiltinVar), ("y", BuiltinVar), ("block", BuiltinVar), ("rotation", BuiltinVar), ("config", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy []
                  }

ucontrolGetBlockFunction :: BuiltinFunction
ucontrolGetBlockFunction =
  BuiltinFunction { builtinFunctionName = "ucontrolGetBlock"
                  , builtinFunctionParams = [ ("x", BuiltinVar), ("y", BuiltinVar)]
                  , builtinFunctionRetType = TupleTy [VarTy, VarTy]
                  }

ucontrolWithinFunction :: BuiltinFunction
ucontrolWithinFunction =
  BuiltinFunction { builtinFunctionName = "ucontrolWithin"
                  , builtinFunctionParams = [ ("x", BuiltinVar), ("y", BuiltinVar), ("radius", BuiltinVar) ]
                  , builtinFunctionRetType = VarTy
                  }

uradarFunction :: BuiltinFunction
uradarFunction =
  BuiltinFunction { builtinFunctionName = "uradar"
                  , builtinFunctionParams = [ ("target1", BuiltinTarget), ("target2", BuiltinTarget)
                                            , ("target3", BuiltinTarget), ("order", BuiltinVar), ("sort", BuiltinSort) ]
                  , builtinFunctionRetType = VarTy
                  }

ulocateOreFunction :: BuiltinFunction
ulocateOreFunction =
  BuiltinFunction { builtinFunctionName = "ulocateOre"
                  , builtinFunctionParams = [ ("ore", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy [VarTy, VarTy, VarTy]
                  }

ulocateBuildingFunction :: BuiltinFunction
ulocateBuildingFunction =
  BuiltinFunction { builtinFunctionName = "ulocateBuilding"
                  , builtinFunctionParams = [ ("buildingGroup", BuiltinBuildingGroup), ("enemy", BuiltinVar) ]
                  , builtinFunctionRetType = TupleTy [VarTy, VarTy, VarTy, VarTy]
                  }

ulocateSpawnFunction :: BuiltinFunction
ulocateSpawnFunction =
  BuiltinFunction { builtinFunctionName = "ulocateSpawn"
                  , builtinFunctionParams = []
                  , builtinFunctionRetType = TupleTy [VarTy, VarTy, VarTy, VarTy]
                  }

ulocateDamagedFunction :: BuiltinFunction
ulocateDamagedFunction =
  BuiltinFunction { builtinFunctionName = "ulocateDamaged"
                  , builtinFunctionParams = []
                  , builtinFunctionRetType = TupleTy [VarTy, VarTy, VarTy, VarTy]
                  }



---

idivFunction :: BuiltinFunction
idivFunction =
  BuiltinFunction { builtinFunctionName = "idiv"
                  , builtinFunctionParams = [("arg1", BuiltinVar), ("arg2", BuiltinVar)]
                  , builtinFunctionRetType = VarTy
                  }

maxFunction :: BuiltinFunction
maxFunction =
  BuiltinFunction { builtinFunctionName = "max"
                  , builtinFunctionParams = [("arg1", BuiltinVar), ("arg2", BuiltinVar)]
                  , builtinFunctionRetType = VarTy
                  }

minFunction :: BuiltinFunction
minFunction =
  BuiltinFunction { builtinFunctionName = "min"
                  , builtinFunctionParams = [("arg1", BuiltinVar), ("arg2", BuiltinVar)]
                  , builtinFunctionRetType = VarTy
                  }

atan2Function :: BuiltinFunction
atan2Function =
  BuiltinFunction { builtinFunctionName = "atan2"
                  , builtinFunctionParams = [("arg1", BuiltinVar), ("arg2", BuiltinVar)]
                  , builtinFunctionRetType = VarTy
                  }

dstFunction :: BuiltinFunction
dstFunction =
  BuiltinFunction { builtinFunctionName = "dst"
                  , builtinFunctionParams = [("arg1", BuiltinVar), ("arg2", BuiltinVar)]
                  , builtinFunctionRetType = VarTy
                  }

noiseFunction :: BuiltinFunction
noiseFunction =
  BuiltinFunction { builtinFunctionName = "noise"
                  , builtinFunctionParams = [("arg1", BuiltinVar), ("arg2", BuiltinVar)]
                  , builtinFunctionRetType = VarTy
                  }


absFunction :: BuiltinFunction
absFunction =
  BuiltinFunction { builtinFunctionName = "abs"
                  , builtinFunctionParams = [("arg1", BuiltinVar)]
                  , builtinFunctionRetType = VarTy
                  }

logFunction :: BuiltinFunction
logFunction =
  BuiltinFunction { builtinFunctionName = "log"
                  , builtinFunctionParams = [("arg1", BuiltinVar)]
                  , builtinFunctionRetType = VarTy
                  }

log10Function :: BuiltinFunction
log10Function =
  BuiltinFunction { builtinFunctionName = "log10"
                  , builtinFunctionParams = [("arg1", BuiltinVar)]
                  , builtinFunctionRetType = VarTy
                  }

sinFunction :: BuiltinFunction
sinFunction =
  BuiltinFunction { builtinFunctionName = "sin"
                  , builtinFunctionParams = [("arg1", BuiltinVar)]
                  , builtinFunctionRetType = VarTy
                  }
cosFunction :: BuiltinFunction
cosFunction =
  BuiltinFunction { builtinFunctionName = "cos"
                  , builtinFunctionParams = [("arg1", BuiltinVar)]
                  , builtinFunctionRetType = VarTy
                  }
tanFunction :: BuiltinFunction
tanFunction =
  BuiltinFunction { builtinFunctionName = "tan"
                  , builtinFunctionParams = [("arg1", BuiltinVar)]
                  , builtinFunctionRetType = VarTy
                  }

floorFunction :: BuiltinFunction
floorFunction =
  BuiltinFunction { builtinFunctionName = "floor"
                  , builtinFunctionParams = [("arg1", BuiltinVar)]
                  , builtinFunctionRetType = VarTy
                  }

ceilFunction :: BuiltinFunction
ceilFunction =
  BuiltinFunction { builtinFunctionName = "ceil"
                  , builtinFunctionParams = [("arg1", BuiltinVar)]
                  , builtinFunctionRetType = VarTy
                  }

sqrtFunction :: BuiltinFunction
sqrtFunction =
  BuiltinFunction { builtinFunctionName = "sqrt"
                  , builtinFunctionParams = [("arg1", BuiltinVar)]
                  , builtinFunctionRetType = VarTy
                  }

randFunction :: BuiltinFunction
randFunction =
  BuiltinFunction { builtinFunctionName = "rand"
                  , builtinFunctionParams = [("arg1", BuiltinVar)]
                  , builtinFunctionRetType = VarTy
                  }

builtinFunctions :: [BuiltinFunction]
builtinFunctions = [readFunction, writeFunction, drawClearFunction, drawColorFunction, drawStrokeFunction, drawLineFunction, drawRectFunction, drawLineRectFunction, drawPolyFunction, drawLinePolyFunction, drawTriangleFunction, drawImageFunction, printFunction, drawFlushFunction, printFlushFunction, getLinkFunction, controlEnabledFunction, controlShootFunction, controlShootpFunction, controlConfigureFunction, radarFunction, sensorFunction, ubindFunction, ucontrolStopFunction, ucontrolMoveFunction, ucontrolApproachFunction, ucontrolBoostFunction, ucontrolPathfindFunction, ucontrolTargetFunction, ucontrolTargetpFunction, ucontrolItemDropFunction, ucontrolItemTakeFunction, ucontrolPayDropFunction, ucontrolPayTakeFunction, ucontrolMineFunction, ucontrolFlagFunction, ucontrolBuildFunction, ucontrolGetBlockFunction, ucontrolWithinFunction, uradarFunction, ulocateOreFunction, ulocateBuildingFunction, ulocateSpawnFunction, ulocateDamagedFunction, idivFunction, maxFunction, minFunction, atan2Function, dstFunction, noiseFunction, absFunction, logFunction, log10Function, sinFunction, cosFunction, tanFunction, floorFunction, ceilFunction, sqrtFunction, randFunction]

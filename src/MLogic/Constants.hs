module MLogic.Constants where

import MLogic.Assembly.Operand

nullC :: Operand
nullC = VarOperand "null"

thisC :: Operand
thisC = VarOperand "@this"

thisxC :: Operand
thisxC = VarOperand "@thisx"

thisyC :: Operand
thisyC = VarOperand "@thisy"

counterC :: Operand
counterC = VarOperand "@counter"

linksC :: Operand
linksC = VarOperand "@links"

falseC :: Operand
falseC = VarOperand "false"

trueC :: Operand
trueC = VarOperand "true"

unitC :: Operand
unitC = VarOperand "@unit"

timeC :: Operand
timeC = VarOperand "@time"

mapwC :: Operand
mapwC = VarOperand "@mapw"

maphC :: Operand
maphC = VarOperand "@maph"

totalItemsC :: Operand
totalItemsC = VarOperand "@totalItems"

firstItemC :: Operand
firstItemC = VarOperand "@firstItem"

totalLiquidsC :: Operand
totalLiquidsC = VarOperand "@totalLiquids"

totalPowerC :: Operand
totalPowerC = VarOperand "@totalPower"

itemCapacityC :: Operand
itemCapacityC = VarOperand "@itemCapacity"

liquidCapacityC :: Operand
liquidCapacityC = VarOperand "@liquidCapacity"

powerCapacityC :: Operand
powerCapacityC = VarOperand "@powerCapacity"

powerNetStoredC :: Operand
powerNetStoredC = VarOperand "@powerNetStored"

powerNetCapacityC :: Operand
powerNetCapacityC = VarOperand "@powerNetCapacity"

powerNetInC :: Operand
powerNetInC = VarOperand "@powerNetIn"

powerNetOutC :: Operand
powerNetOutC = VarOperand "@powerNetOut"

ammoC :: Operand
ammoC = VarOperand "@ammo"

ammoCapacityC :: Operand
ammoCapacityC = VarOperand "@ammoCapacity"

healthC :: Operand
healthC = VarOperand "@health"

maxHealthC :: Operand
maxHealthC = VarOperand "@maxHealth"

heatC :: Operand
heatC = VarOperand "@heat"

efficiencyC :: Operand
efficiencyC = VarOperand "@efficiency"

timescaleC :: Operand
timescaleC = VarOperand "@timescale"

rotationC :: Operand
rotationC = VarOperand "@rotation"

xC :: Operand
xC = VarOperand "@x"

yC :: Operand
yC = VarOperand "@y"

shootXC :: Operand
shootXC = VarOperand "@shootX"

shootYC :: Operand
shootYC = VarOperand "@shootY"

sizeC :: Operand
sizeC = VarOperand "@size"

deadC :: Operand
deadC = VarOperand "@dead"

rangeC :: Operand
rangeC = VarOperand "@range"

shootingC :: Operand
shootingC = VarOperand "@shooting"

boostingC :: Operand
boostingC = VarOperand "@boosting"

mineXC :: Operand
mineXC = VarOperand "@mineX"

mineYC :: Operand
mineYC = VarOperand "@mineY"

miningC :: Operand
miningC = VarOperand "@mining"

teamC :: Operand
teamC = VarOperand "@team"

typeC :: Operand
typeC = VarOperand "@type"

flagC :: Operand
flagC = VarOperand "@flag"

controlledC :: Operand
controlledC = VarOperand "@controlled"

controllerC :: Operand
controllerC = VarOperand "@controller"

-- deprecated
commandedC :: Operand
commandedC = VarOperand "@commanded"


nameC :: Operand
nameC = VarOperand "@name"

configC :: Operand
configC = VarOperand "@config"

payloadCountC :: Operand
payloadCountC = VarOperand "@payloadCount"

payloadTypeC :: Operand
payloadTypeC = VarOperand "@payloadType"

enabledC :: Operand
enabledC = VarOperand "@enabled"

configureC :: Operand
configureC = VarOperand "@configure"
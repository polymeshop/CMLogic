module MLogic.Assembly.Op where


import MLogic.Assembly.Operand
import MLogic.Constants

data OpCode = OpRead | OpWrite | OpDraw | OpPrint | OpDrawFlush
            | OpPrintFlush | OpGetlink | OpControl | OpRadar | OpSensor
            | OpSet | OpOp | OpEnd | OpJump | OpUBind | OpUControl | OpURadar
            | OpULocate 
-- pseudo ops
            | OpLabel
            deriving Eq


data Draw = DrawClear Operand Operand Operand
--                    r       g       b
          | DrawColor Operand Operand Operand Operand
--                    r       g       b       a
          | DrawStroke Operand
--                     line width
          | DrawLine Operand Operand Operand Operand
--                   x       y       x2      y2
          | DrawRect Operand Operand Operand Operand
--                   x       y       width   height
          | DrawLineRect Operand Operand Operand Operand
--                       x       y       width   height
          | DrawPoly Operand Operand Operand Operand Operand
--                   x       y       sides   radius  rotation
          | DrawLinePoly Operand Operand Operand Operand Operand
--                       x       y       sides   radius  rotation
          | DrawTriangle Operand Operand Operand Operand Operand Operand
--                       x       y       x2      y2      x3      y3
          | DrawImage Operand Operand Operand Operand Operand
--                    x       y       image   size    rotation
-- other ones not supported yet
    deriving Eq
instance Show Draw where
  show (DrawClear r g b) = "clear " <> show r <> " " <> show g <> " " <> show b
  show (DrawColor r g b a) = "color " <> unwords (map show [r,g,b,a])
  show (DrawStroke width) = "width " <> show width
  show (DrawLine x y x2 y2) = "line " <> unwords (map show [x,y,x2,y2])
  show (DrawRect x y width height) = "rect " <> unwords (map show [x,y,width,height])
  show (DrawLineRect x y width height) = "lineRect " <> unwords (map show [x,y,width,height])
  show (DrawPoly x y sides radius rotation) = "poly " <> unwords (map show [x,y,sides,radius,rotation])
  show (DrawLinePoly x y sides radius rotation) = "poly " <> unwords (map show [x,y,sides,radius,rotation])
  show (DrawTriangle x y x2 y2 x3 y3) = "triangle " <> unwords (map show [x,y,x2,y2,x3,y3])
  show (DrawImage x y image size rotation) = "image " <> unwords (map show [x,y,image,size,rotation])
data Control = CEnabled Operand Operand
--                      obj     value
             | CShoot   Operand Operand Operand Operand
--                      obj     x       y       shoot
             | CShootp  Operand Operand Operand
--                      obj     unit    shoot
             | CConfigure Operand Operand
--                        obj     val
            deriving Eq
instance Show Control where
  show (CEnabled obj val) = "enabled " <> unwords (map show [obj, val])
  show (CShoot obj x y shoot) = "shoot " <> unwords (map show [obj, x, y, shoot])
  show (CShootp obj unit shoot) = "shootp " <> unwords (map show [obj, unit, shoot])
  show (CConfigure obj val) = "configure " <> unwords (map show [obj, val])

data UControl = UStop
              | UMove Operand Operand
              --      x       y
              | UApproach Operand Operand Operand
              --          x       y       radius
              | UBoost Operand
              --       enabled
              | UPathfind
              | UTarget Operand Operand Operand
              --        x       y       shoot
              | UTargetp Operand Operand
              --         unit    shoot
              | UItemDrop Operand Operand
              --          to      amount
              | UItemTake Operand Operand Operand
              --          from    item    amount
              | UPayDrop
              | UPayTake Operand
              --         takeUnits
              | UMine Operand Operand
              --      x       y
              | UFlag Operand
              --      value
              | UBuild Operand Operand Operand Operand    Operand
              --       x       y       block   rotation   config
              | UGetBlock Operand Operand Operand Operand
              --          x       y       type    building
              | UWithin Operand Operand Operand Operand
              --        x       y       radius  result
  deriving Eq
instance Show UControl where
 show UStop = "stop"
 show (UMove x y) = "move " <> unwords (map show [x, y])
 show (UApproach x y radius) = "approach " <> unwords (map show [x, y, radius])
 show (UBoost enabled) = "boost " <> show enabled
 show UPathfind = "pathfind"
 show (UTarget x y shoot) = "target " <> unwords (map show [x, y, shoot])
 show (UTargetp unit shoot) = "targetp " <> unwords (map show [unit, shoot])
 show (UItemDrop to amount) = "itemDrop " <> unwords (map show [to, amount])
 show (UItemTake from item amount) = "itemTake " <> unwords (map show [from, item, amount])
 show UPayDrop = "payDrop"
 show (UPayTake takeUnits) = "payTake " <> show takeUnits
 show (UMine x y) = "mine " <> unwords (map show [x, y])
 show (UFlag value) = "flag " <> show value
 show (UBuild x y block rotation config) = "build " <> unwords (map show [x, y, block, rotation, config])
 show (UGetBlock x y ty building) = "getBlock " <> unwords (map show [x, y, ty, building])
 show (UWithin x y radius result) = "within " <> unwords (map show [x, y, radius, result])


data Target = AnyT | EnemyT | AllyT | PlayerT | AttackerT
            | FlyingT | BossT | GroundT
  deriving Eq
instance Show Target where
  show AnyT = "any"
  show EnemyT = "enemy"
  show AllyT = "ally"
  show PlayerT = "player"
  show AttackerT = "attacker"
  show FlyingT = "flying"
  show BossT = "boss"
  show GroundT = "ground"

data SortBy = Distance | Health | Shield | Armor | MaxHealth deriving Eq

instance Show SortBy where
  show Distance = "distance"
  show Health = "health"
  show Shield = "shield"
  show Armor = "armor"
  show MaxHealth = "maxHealth"

data Jump = JEq Operand Operand
          | JSEq Operand Operand -- strictEqual
          | JNeq Operand Operand
          | JLt Operand Operand
          | JLe Operand Operand
          | JGt Operand Operand
          | JGe Operand Operand
          | JAlways
  deriving Eq
instance Show Jump where
  show (JEq a b) = "equal " <> unwords (map show [a, b])
  show (JSEq a b) = "strictEqual " <> unwords (map show [a, b])  
  show (JNeq a b) = "notEqual " <> unwords (map show [a, b])
  show (JLt a b) = "lessThan " <> unwords (map show [a, b])
  show (JLe a b) = "lessThanEq " <> unwords (map show [a, b])
  show (JGt a b) = "greaterThan " <> unwords (map show [a, b])
  show (JGe a b) = "greaterThanEq " <> unwords (map show [a, b])
  show JAlways = "always"

data JumpTarget = IntTarget Integer
                | LabTarget String
  deriving Eq
instance Show JumpTarget where
  show (IntTarget i) = show i
  show (LabTarget s) = "lab:" <> s

data ULocateBuildingType = Core | Storage | Generator | Turret | Factory | Repair | Rally | Battery | Resupply | Reactor
  deriving Eq

instance Show ULocateBuildingType where
  show Core = "core"
  show Storage = "storage"
  show Generator = "generator"
  show Turret = "turret"
  show Factory = "factory"
  show Repair = "repair"
  show Rally = "rally"
  show Battery = "battery"
  show Resupply = "resupply"
  show Reactor = "reactor"

data ULocate = ULocateOre Operand Operand Operand Operand
             --           ore     outX    outY    found
             | ULocateBuilding Operand Operand     Operand Operand Operand Operand
             --                type    enemy(bool) outX    outY    found   building
             | ULocateSpawn Operand Operand Operand Operand
             --             outX    outY    found   building
             | ULocateDamaged Operand Operand Operand Operand
             --               outX    outY    found   building
  deriving Eq
instance Show ULocate where
  show (ULocateOre ore outX outY found)
      = "ore core true " <> unwords (map show [ore, outX, outY, found, VarOperand "building"])
  show (ULocateBuilding ty enemy outX outY found building)
      = "building " <> show ty <> " " <> unwords (map show [enemy, nullC, outX, outY, found, building])
  show (ULocateSpawn outX outY found building)
      = "spawn core true " <> unwords (map show [nullC, outX, outY, found, building])
  show (ULocateDamaged outX outY found building)
      = "damaged core true " <> unwords (map show [nullC, outX, outY, found, building])

data Instr
           = ReadI Operand Operand Operand
           | WriteI Operand Operand Operand
           | DrawI Draw
           | PrintI Operand
           | DrawFlushI Operand
           | PrintFlushI Operand
           | GetlinkI Operand Operand
           | ControlI Control
           | RadarI Operand Operand Operand Operand Operand Operand Operand
--                  from                            order          output
           | SensorI Operand Operand Operand
--                   output  obj     property
           | SetI Operand Operand
--                output  value
           | OpI String [Operand]
           | EndI
           | JumpI JumpTarget Jump
           | UBindI Operand
           | UControlI UControl
           | URadarI Operand Operand Operand Operand Operand Operand
--                                           order          output
           | ULocateI ULocate
--         psuedo ops
           | LabelI String
   deriving Eq
instance Show Instr where
  show (ReadI out cell pos) = "read " <> unwords (map show [out, cell, pos])
  show (WriteI out cell pos) = "write " <> unwords (map show [out, cell, pos])
  show (DrawI d) = "draw " <> show d
  show (PrintI s) = "print " <> show s
  show (DrawFlushI s) = "drawflush " <> show s
  show (PrintFlushI s) = "printflush " <> show s
  show (GetlinkI r num) = "getlink " <> unwords (map show [r, num])
  show (ControlI cont) = "control " <> show cont
  show (RadarI from t1 t2 t3 order sb out) = "radar " <> unwords (map show [t1, t2, t3]) <> " " <> show sb <> " " <>  unwords (map show [from, order, out])
  show (SensorI out obj prop) = "sensor " <> unwords (map show [out, obj, prop])
  show (SetI out val) = "set " <> unwords (map show [out, val])
  show (OpI op ops) = "op " <> op <> " " <> unwords (map show ops)
  show EndI = "end"
  show (JumpI jt cond) = "jump " <> show jt <> " " <> show cond
  show (UBindI op) = "ubind " <> show op
  show (UControlI cont) = "ucontrol " <> show cont
  show (URadarI t1 t2 t3 order sb out) = "uradar " <> unwords (map show [t1, t2, t3]) <> " " <> show sb <> " " <> unwords (map show [DoubleOperand 0, order, out])
  show (ULocateI locate) = "ulocate " <> show locate
  show (LabelI s) = "lab: " <> s


class GetOp a where
  getOp :: a -> OpCode

instance GetOp Instr where
  getOp (ReadI _ _ _) = OpRead
  getOp (WriteI _ _ _) = OpWrite
  getOp (DrawI _) = OpDraw
  getOp (PrintI _) = OpPrint
  getOp (DrawFlushI _) = OpDrawFlush
  getOp (PrintFlushI _) = OpPrintFlush
  getOp (GetlinkI _ _) = OpGetlink
  getOp (ControlI _) = OpControl
  getOp (RadarI _ _ _ _ _ _ _) = OpRadar
  getOp (SensorI _ _ _) = OpSensor
  getOp (SetI _ _) = OpSet
  getOp (OpI _ _) = OpOp
  getOp EndI = OpEnd
  getOp (JumpI _ _) = OpJump
  getOp (UBindI _) = OpUBind
  getOp (UControlI _) = OpUControl
  getOp (URadarI _ _ _ _ _ _) = OpURadar
  getOp (ULocateI _) = OpULocate
  getOp (LabelI _) = OpLabel
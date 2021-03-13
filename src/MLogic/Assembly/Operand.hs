module MLogic.Assembly.Operand where

data Operand = VarOperand String
             | StringOperand String
             | DoubleOperand Double
   deriving Eq
class ToOperand a where
  toOperand :: a -> Operand

instance ToOperand Operand where
  toOperand = id

instance ToOperand Double where
  toOperand = DoubleOperand

instance Show Operand where
  show (VarOperand v) = v
  show (StringOperand s) = show s
  show (DoubleOperand d) = show d

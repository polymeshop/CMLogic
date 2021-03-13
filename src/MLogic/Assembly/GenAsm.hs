module MLogic.Assembly.GenAsm where

import MLogic.Assembly.Operand
import MLogic.Assembly.Op
import MLogic.Assembly.Optimize.Optimize
import MLogic.Assembly.GenAsm.Utils
import MLogic.Assembly.GenAsm.Types

import Control.Monad.Identity

import Data.Set (Set)
import qualified Data.Set as S

import Data.Endo

import Data.Map (Map)
import qualified Data.Map as M

import MLogic.Assembly.Optimize.Types
import Debug.Trace

-- data GenAsmOption = GenAsmIns | GenAsmReplaceLabel | GenAsmReplaceLabelOpt maybe?

getLabelLoc :: [Instr] -> Map String Integer
getLabelLoc ins =
  let aux :: Integer -> [Instr] -> Map String Integer -> Map String Integer
      aux i (LabelI s:xs) m = aux i xs (M.insert s i m)
      aux i (_:xs) m = aux (i+1) xs m
      aux _ [] m = m
  in aux 0 ins M.empty


replaceLabelWithLoc :: Map String Integer -> Instr -> Instr
replaceLabelWithLoc m i@(JumpI (LabTarget s) cond) =
  case M.lookup s m of
    Just loc -> JumpI (IntTarget loc) cond
    Nothing -> i
replaceLabelWithLoc _ i = i


{-
genAsm :: IsInstr i GenAsm () -> IO (Either String [Instr])
genAsm prog = do
  ins <- genAsmNoReplaceLabel prog
  let
    go ins =
      let labelLoc = getLabelLoc ins
      in map (replaceLabelWithLoc labelLoc) ins
  return $ fmap (filter ((/= OpLabel) . getOp) . go) ins


genAsmNoReplaceLabel :: IsInstr i => GenAsmM i Identity () -> GenAsmState i -> Either String [Instr]
genAsmNoReplaceLabel prog = do
  let initState = GenAsmState { genAsmVarCounter = M.empty
                              , genAsmLabVar = 0
                              , genAsmInstrs = Endo id
                              , genAsmVars = S.empty
                              }
      
      r = runIdentity $ flip runGenAsmM initState $ do
        _ <- addNewLab -- for optimization code
        prog
  case r of
    Left err -> return (Left err)
    Right (s, ()) -> return $ Right $ runEndo (genAsmInstrs s) []


genAsmNoReplaceLabel :: GenAsm () -> IO (Either String [Instr])
genAsmNoReplaceLabel prog = do
  let initState = GenAsmState { genAsmVarCounter = M.empty
                              , genAsmLabVar = 0
                              , genAsmInstrs = Endo id
                              , genAsmVars = S.empty
                              }
      
      r = runIdentity $ flip runGenAsmM initState $ do
        _ <- addNewLab -- for optimization code
        prog
  case r of
    Left err -> return (Left err)
    Right (s, ()) ->
      let asmOptInitState = AsmOptState { labNum = genAsmLabVar s }
      in do
        (compState', ins) <- runAsmOpt (optimize $ runEndo (genAsmInstrs s) []) asmOptInitState
        case last ins of
          LabelI lab ->
            let go (j@(JumpI (LabTarget lab') cond):xs) = case lab == lab' of
                                                        True -> JumpI (IntTarget 0) cond:go xs
                                                        False -> j:go xs
                go (x:xs) = x:go xs
                go [] = []
            in return $ Right (go ins)
          _ -> return $ Right ins
-}
module MLogic.Assembly.Optimize.Optimize where

import MLogic.Assembly.Optimize.Types

import MLogic.Assembly.Op
import MLogic.Assembly.Optimize.Redundant
import MLogic.Assembly.Optimize.MergeLabels

import System.IO

import Control.Monad
import Control.Monad.Trans

optimizationPasses :: [(String, [Instr] -> AsmOpt [Instr])]
optimizationPasses =
  [ ("mergeLabels", return . mergeLabels)
  , ("removeRedundantJump", return . removeRedundantJump)
  , ("removeRedundantPrint", return . removeRedundantPrint)
  , ("removeUnreachableIns", return . removeUnreachableIns)
  , ("removeRedundantBlock", removeRedundantBlock)
  ]

applyPass :: [Instr] -> [(String, [Instr] -> AsmOpt [Instr])] -> (String -> AsmOpt ()) -> AsmOpt [Instr]
applyPass ins ((name, f):xs) logger = do
  ins' <- f ins
  when (ins /= ins') $ do
    logger $ "-- applying opt pass " <> name <> " -- "
    logger $ unlines $ map show ins'
    logger $ "--------------------"
  applyPass ins' xs logger
applyPass ins [] _ = return ins

optimize :: [Instr] -> AsmOpt [Instr]
optimize ins = do
  ins' <- applyPass ins optimizationPasses (liftIO . hPutStrLn stderr)
  if ins /= ins' then optimize ins' else return ins'
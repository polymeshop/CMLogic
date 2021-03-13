module MLogic.Assembly.Optimize.MergeLabels where

import Data.Map (Map)
import qualified Data.Map as M

import MLogic.Assembly.Op

getLabClasses :: ([String], [[String]]) -> [Instr] -> [[String]]
getLabClasses (cur, clss) ((LabelI lab):xs)  = getLabClasses (lab:cur, clss) xs
getLabClasses (cur, clss) (_:xs) = getLabClasses ([], cur:clss) xs
getLabClasses (cur, clss) [] = cur:clss


mergeLabels :: [Instr] -> [Instr]
mergeLabels ins =
  let
    labClasses = getLabClasses ([], []) ins
    pivots = map head labClasses
    pivMap = M.fromList $ concat
                $ map (\ (piv, cls) -> map (\c -> (c, piv)) cls) 
                $ zip pivots labClasses

    -- only keep pivot labels and rewrite the jumps to the pivot labels
    go ((LabelI lab):xs) = case M.lookup lab pivMap of
                             Just piv -> if lab == piv
                                         then LabelI piv:go xs
                                         else go xs
                             Nothing -> error $ "Failed to find label " <> lab <> " in pivMap"
    go ((JumpI (LabTarget lt) jmpCond):xs) =
                case M.lookup lt pivMap of
                  Just piv -> JumpI (LabTarget piv) jmpCond:go xs
                  Nothing -> error $ "Failed to find label " <> lt <> " in pivMap"
    go (x:xs) = x:go xs
    go [] = []
  in go ins
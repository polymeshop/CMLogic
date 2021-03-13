module MLogic.Assembly.Optimize.Redundant where

import MLogic.Assembly.Operand
import MLogic.Assembly.Op

import Data.List

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

import MLogic.Assembly.Optimize.Types

-- assumes that all jumps are jumps to labels -- don't violate this assumption

removeRedundantJump :: [Instr] -> [Instr]
removeRedundantJump e@(JumpI (LabTarget lab) _:(LabelI lab'):ys)
   | lab == lab' = removeRedundantJump (drop 1 e)
   | otherwise   = take 2 e ++ removeRedundantJump ys
removeRedundantJump (x:xs) = x:removeRedundantJump xs
removeRedundantJump [] = []



findBlocks :: [Instr] -> [(String, [Instr])]
findBlocks ins =
  let go ((LabelI lab):rest) =
         let (cur, acc) = go rest
         in ([], (lab, cur):acc)
      go (x:rest) =
         let (cur, acc) = go rest
         in (x:cur, acc)
      go [] = ([], [])
  in case go ins of
       (r@(x:xs), _) -> error $ "unhandled in findBlocks -- the first instruction should be a label:\n" <> unlines (map show r)
       ([], acc) -> acc


removeUnreachableIns :: [Instr] -> [Instr]
removeUnreachableIns ins =
  let blocks = findBlocks ins
      cleanUpBlock (j@(JumpI (LabTarget lab) JAlways):xs) = [j]
      cleanUpBlock (x:xs) = x:cleanUpBlock xs
      cleanUpBlock [] = []
  in concat $ map (\ (lab, ins) -> LabelI lab:cleanUpBlock ins) blocks

data Target = T String | Loop

-- a `block` is a list of instructions containing no labels but is surronded by label
removeRedundantBlock :: [Instr] -> AsmOpt [Instr]
removeRedundantBlock ins@(LabelI lab:_) =
  let blocks = findBlocks ins
      isTrivBlock (_, ins') = case ins' of
                               (JumpI (LabTarget _) JAlways:[]) -> True
                               _ -> False
      trivBlocks = filter isTrivBlock blocks
      trivLabs = map fst trivBlocks
      -- trivLabsSet = S.fromList trivLabs
      getEdge (lab, JumpI (LabTarget lab') JAlways:[]) = (lab, lab')
      getEdge _ = error "unhandled in getEdge"
      edges = M.fromList $ map getEdge trivBlocks
      traverseEdge startNode currentNode path
        = if startNode == currentNode
          then (Loop, path)
          else case M.lookup currentNode edges of
                 Just next -> traverseEdge startNode next (currentNode:path)
                 Nothing   -> (T currentNode, path)
      targetMap = foldl' (\acc elem -> case M.lookup elem acc of
                             Just target -> M.insert elem target acc
                             Nothing     -> case M.lookup elem edges of
                                              Just nextEdge -> let (target, path) = traverseEdge elem nextEdge [elem]
                                                               in foldl' (\ acc' elem' -> M.insert elem' target acc') acc path
                                              Nothing -> error "unhandled in removeRedundantBlock"
         ) M.empty trivLabs
      go :: Maybe String -> [Instr] -> AsmOpt [Instr]
      go _ ((LabelI lab):rest@(x:xs)) =
         case M.lookup lab targetMap of
           Just Loop       -> (\s -> LabelI lab:JumpI (LabTarget lab) JAlways:s) <$> go Nothing xs
           Just (T target) -> (JumpI (LabTarget target) JAlways:) <$> go Nothing xs
           Nothing         -> (LabelI lab:) <$> go (Just lab) rest
      go prevLab (j@(JumpI (LabTarget lab) cond):rest) =
        case M.lookup lab targetMap of
          Just Loop       -> case prevLab of
                               Just prevLab' ->
                                 (JumpI (LabTarget prevLab') cond:) <$> go Nothing rest
                               Nothing -> do
                                 lab' <- newLab
                                 (\s -> LabelI lab':JumpI (LabTarget lab') cond:s) <$> go Nothing rest
          Just (T target) -> (JumpI (LabTarget target) cond:) <$> go Nothing rest
          Nothing         -> (j:) <$> go Nothing rest
      go _ (x:xs) = (x:) <$> go Nothing xs
      go _ [] = return []
  in do
    ins' <- go Nothing ins
    case ins' of
      (LabelI _:_) -> return ins'
      (_:_) -> return (LabelI lab:ins')
      [] -> return []
removeRedundantBlock [] = return []

removeRedundantPrint :: [Instr] -> [Instr]
removeRedundantPrint ins = filter (/= (PrintI (StringOperand ""))) ins

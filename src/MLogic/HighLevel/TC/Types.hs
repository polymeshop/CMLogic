{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module MLogic.HighLevel.TC.Types where

import Control.Monad.State
import Control.Monad.Except


import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

import MLogic.HighLevel.Types
import Text.Parsec (SourcePos)

import System.IO

type Level = Integer


data TCState = TCState { tcInputVarSet :: Set String
                       , tcInputVarMap :: Map String (Maybe CL)
                       , tcVarMap :: Map String (Types, String, SourcePos, Level)
                         --                             varDesc                 
                       , tcDeclaredLinkConstantsMap :: Map String SourcePos
                       , tcBuiltinFunctionMap :: Map String BuiltinFunction
                       , tcFunctionSigMap :: Map String ([(String, SourcePos, Types)], Types)
                                                       --  params                      ret type
                       , tcTopLevelNamesMap :: Map String (String, Maybe SourcePos)
                       , tcTopLevelReservedNamesMap :: Map String (String, Maybe SourcePos)
                       , tcInBreakable :: Bool
                       , tcScopeLevel :: Integer
                       , tcLogger :: String -> IO ()
                       }

type TC = StateT TCState (ExceptT String IO)

initTCState = TCState { tcInputVarSet = S.empty
                      , tcInputVarMap = M.empty
                      , tcVarMap = M.empty
                      , tcDeclaredLinkConstantsMap = M.empty
                      , tcBuiltinFunctionMap = M.empty
                      , tcFunctionSigMap = M.empty
                      , tcTopLevelNamesMap = M.empty
                      , tcTopLevelReservedNamesMap = M.empty
                      , tcInBreakable = False
                      , tcScopeLevel = 0
                      , tcLogger = hPutStrLn stderr }

runTC :: TCState -> TC a -> IO (Either String (a, TCState))
runTC st prog = runExceptT $ runStateT prog st

class TCMsg a where
  tcMsg :: a -> String

instance TCMsg String where
  tcMsg = id

instance TCMsg [String] where
  tcMsg = unlines
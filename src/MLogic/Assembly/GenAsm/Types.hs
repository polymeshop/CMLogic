{-# LANGUAGE MultiParamTypeClasses, NamedFieldPuns #-} 
module MLogic.Assembly.GenAsm.Types where

import Control.Monad.Identity
import Control.Monad.State.Class

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

import MLogic.Assembly.Op

import Data.Endo

data GenAsmState i =
  GenAsmState { genAsmVarCounter :: Map String Integer
              , genAsmLabVar :: Integer
              , genAsmInstrs :: Endo [i]
              , genAsmVars :: Set String
              }

data GenAsmM i m a = GenAsmM { runGenAsmM :: GenAsmState i ->
                                          m (Either String (GenAsmState i, a)) }



type GenAsm = GenAsmM Instr Identity

instance Monad m => Monad (GenAsmM i m) where
  return a = GenAsmM (\s -> return (Right (s, a)))
  m >>= f = GenAsmM $ \s -> do
               r <- runGenAsmM m s
               case r of
                 Right (s', a)  -> runGenAsmM (f a) s'
                 Left s -> return (Left s)

instance Monad m => Applicative (GenAsmM i m) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

instance Monad m => Functor (GenAsmM i m) where
  fmap f fa = do
    a <- fa
    return (f a)

instance Monad m => MonadState (GenAsmState i) (GenAsmM i m) where
  get = GenAsmM $ \s -> return (Right (s, s))
  put st = GenAsmM $ \s -> return (Right (st, ()))

instance Monad m => MonadFail (GenAsmM i m) where
  fail s = GenAsmM $ \_ -> return (Left s)



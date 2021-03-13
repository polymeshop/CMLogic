{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module MLogic.Assembly.Optimize.Types where

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Trans

data AsmOptState = AsmOptState { labNum :: Integer }

data AsmOptM m a = AsmOptM { runAsmOpt :: AsmOptState -> m (AsmOptState, a) }

type AsmOpt = AsmOptM IO

instance Monad m => Monad (AsmOptM m) where
  return a = AsmOptM (\s -> return (s, a))
  m >>= f = AsmOptM $ \s -> do
    (s', a) <- runAsmOpt m s
    runAsmOpt (f a) s'

instance Monad m => Applicative (AsmOptM m) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)


instance Monad m => Functor (AsmOptM m) where
  fmap f ma = do
    a <- ma
    return (f a)


instance Monad m => MonadState AsmOptState (AsmOptM m) where
  get = AsmOptM $ \s -> return (s, s)
  put st = AsmOptM $ \_ -> return (st, ())

instance MonadIO (AsmOptM IO) where
  liftIO act = AsmOptM $ \s -> do
    r <- act
    return (s, r)
newLab :: Monad m => AsmOptM m String
newLab = do
  AsmOptState { labNum = labNum } <- get
  let labStr = "lab" ++ show labNum
  put (AsmOptState { labNum = labNum + 1 })
  return labStr

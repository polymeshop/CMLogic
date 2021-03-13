module Data.Endo where

newtype Endo a = Endo { runEndo :: a -> a }

composeEndo :: Endo a -> Endo a -> Endo a
composeEndo (Endo g) (Endo f) = Endo (g . f)
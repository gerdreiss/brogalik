module Data.StateT where

newtype StateT s a =
  StateT
    { runStateT :: s -> (a, s)
    }

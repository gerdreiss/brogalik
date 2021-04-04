module Data.StateT where

newtype StateT s a =
  StateT
    { runStateT :: s -> ((), s)
    }

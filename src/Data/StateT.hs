{-# LANGUAGE TupleSections #-}

module Data.StateT where

import           Data.Bifunctor                 ( first )

newtype StateT s m a = StateT
  { runStateT :: s -> m (a, s)
  }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT run) = StateT $ fmap (first f) . run

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)

  (StateT runF) <*> (StateT runX) = StateT $ \s -> do
    (f, s' ) <- runF s
    (x, s'') <- runX s'
    pure (f x, s'')

instance Monad m => Monad (StateT s m) where
  return a = StateT $ \s -> return (a, s)

  (StateT run) >>= f = StateT $ \s -> do
    (a, s') <- run s
    runStateT (f a) s'


getState :: Monad m => StateT s m s
getState = StateT $ \s -> return (s, s)

liftM :: Monad m => m a -> StateT s m a
liftM m = StateT $ \s -> (, s) <$> m

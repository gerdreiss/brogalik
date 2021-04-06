module Data.StateT where

newtype StateT s m a = StateT
  { runStateT :: s -> m (s, a)
  }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT run) = StateT $ fmap (fmap f) . run

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (s, a)

  (StateT runF) <*> (StateT runX) = StateT $ \s -> do
    (s' , f) <- runF s
    (s'', x) <- runX s'
    pure (s'', f x)

instance Monad m => Monad (StateT s m) where
  (StateT run) >>= f = StateT $ \s -> do
    (s', a) <- run s
    runStateT (f a) s'

getState :: Monad m => StateT s m s
getState = StateT $ \s -> return (s, s)

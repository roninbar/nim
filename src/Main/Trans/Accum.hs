{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main.Trans.Accum
  ( AccumT(..)
  , execAccumT
  ) where

import           Control.Monad.Accum    (MonadAccum (..))
import           Control.Monad.Identity (Identity)
import           Control.Monad.IO.Class (MonadIO (..))

newtype AccumT w m a =
  AccumT
    { runAccumT :: w -> m (a, w)
    }

execAccumT :: Monad m => AccumT w m a -> w -> m w
execAccumT (AccumT f) = fmap snd . f

type Accum w = AccumT w Identity

instance Monad m => Functor (AccumT w m) where
  fmap :: (a -> b) -> AccumT w m a -> AccumT w m b
  fmap f (AccumT g) =
    AccumT $ \s -> do
      (a, s') <- g s
      return (f a, s')

instance Monad m => Applicative (AccumT w m) where
  pure :: a -> AccumT w m a
  pure a = AccumT $ \s -> return (a, s)
  (<*>) :: AccumT w m (a -> b) -> AccumT w m a -> AccumT w m b
  AccumT f <*> AccumT g =
    AccumT $ \s -> do
      (a, s') <- f s
      (b, s'') <- g s'
      return (a b, s'')

instance Monad m => Monad (AccumT w m) where
  return :: a -> AccumT w m a
  return a = AccumT $ \s -> return (a, s)
  (>>=) :: AccumT w m a -> (a -> AccumT w m b) -> AccumT w m b
  AccumT f >>= g =
    AccumT $ \s -> do
      (a, s') <- f s
      runAccumT (g a) s'

instance (Monad m, Monoid w) => (MonadAccum w) (AccumT w m) where
  accum :: (w -> (a, w)) -> AccumT w m a
  accum f = AccumT $ \s -> return (f s)
  add :: w -> AccumT w m ()
  add k = AccumT $ \s -> return ((), s `mappend` k)
  look :: AccumT w m w
  look = AccumT $ \s -> return (s, s)

instance MonadIO m => MonadIO (AccumT w m) where
  liftIO :: IO a -> AccumT w m a
  liftIO ioa =
    AccumT $ \s -> do
      a <- liftIO ioa
      return (a, s)

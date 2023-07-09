{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

module Main.Trans.Accum
  ( AccumT(..)
  , execAccumT
  ) where

import           Control.Monad                  ( ap )
import           Control.Monad.Accum            ( MonadAccum(..) )
import           Control.Monad.Identity         ( Identity )
import           Control.Monad.IO.Class         ( MonadIO(..) )

newtype AccumT w m a =
  AccumT
    { runAccumT :: w -> m (a, w)
    }
  deriving (Functor)

evalAccumT :: Monad m => AccumT w m a -> w -> m a
evalAccumT (AccumT f) = fmap fst . f

execAccumT :: Monad m => AccumT w m a -> w -> m w
execAccumT (AccumT f) = fmap snd . f

mapAccumT :: (m (a, w) -> n (b, w)) -> AccumT w m a -> AccumT w n b
mapAccumT f (AccumT g) = AccumT $ f . g

type Accum w = AccumT w Identity

instance (Monad m, Monoid w) => Applicative (AccumT w m) where
  pure :: a -> AccumT w m a
  pure = accum . const . (, mempty)
  (<*>) :: AccumT w m (a -> b) -> AccumT w m a -> AccumT w m b
  (<*>) = ap

instance (Monad m, Monoid w) => Monad (AccumT w m) where
  (>>=) :: AccumT w m a -> (a -> AccumT w m b) -> AccumT w m b
  f >>= g = AccumT $ \w -> do
    (x, u) <- runAccumT f w
    (y, v) <- runAccumT (g x) (w <> u)
    return (y, u <> v)

instance (Monad m, Monoid w) => (MonadAccum w) (AccumT w m) where
  accum :: (w -> (a, w)) -> AccumT w m a
  accum f = AccumT $ return . f
  add :: w -> AccumT w m ()
  add = accum . const . ((), )
  look :: AccumT w m w
  look = accum (, mempty)

instance (MonadIO m, Monoid w) => MonadIO (AccumT w m) where
  liftIO :: IO a -> AccumT w m a
  liftIO ioa = AccumT $ \s -> do
    a <- liftIO ioa
    return (a, mempty)

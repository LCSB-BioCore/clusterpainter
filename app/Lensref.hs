{-# LANGUAGE RankNTypes #-}

module Lensref where

import Data.IORef

import Lens.Micro

withVal :: IORef a -> (a -> IO b) -> IO b
withVal r io = readIORef r >>= io

withRef :: a -> (IORef a -> IO b) -> IO (a, b)
withRef a io = do
  r <- newIORef a
  b <- io r
  a' <- readIORef r
  pure (a', b)

withRef_ :: a -> (IORef a -> IO b) -> IO a
withRef_ a io = do
  r <- newIORef a
  _ <- io r
  readIORef r

withZoom ::
     IORef big
  -> (forall f. Functor f => LensLike' f big small)
  -> (IORef small -> IO a)
  -> IO a
withZoom rb l io = do
  rs <- readIORef rb >>= newIORef . (^. l)
  a <- io rs
  readIORef rs >>= modifyIORef rb . (l .~)
  pure a

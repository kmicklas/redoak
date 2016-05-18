{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Redoak.Language.Base where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Constraint
import Data.Functor.Identity
import Data.Maybe as M

import Control.Comonad.Cofree8
import Data.Functor8
import Data.Traversable8

import Redoak.Event

class Traversable8 f => NonTerminal f where
  -- | Number of children
  length :: forall a0 a1 a2 a3 a4 a5 a6 a7 . f a0 a1 a2 a3 a4 a5 a6 a7 -> Word

  -- | Can we select a range of adjacent children or only a single child at a time?
  canSelectRange :: forall a0 a1 a2 a3 a4 a5 a6 a7 . f a0 a1 a2 a3 a4 a5 a6 a7 -> Bool

  -- | Will `indexC` and `replaceC` work?
  canDescend :: forall a0 a1 a2 a3 a4 a5 a6 a7 . f a0 a1 a2 a3 a4 a5 a6 a7 -> Bool

  indexC :: forall m b  a0 a1 a2 a3 a4 a5 a6 a7
         .  Functor m
         => f a0 a1 a2 a3 a4 a5 a6 a7
         -> Word
         -> (a0 -> m b)         -> (a1 -> m b)
         -> (a2 -> m b)         -> (a3 -> m b)
         -> (a4 -> m b)         -> (a5 -> m b)
         -> (a6 -> m b)         -> (a7 -> m b)
         -> m b

  modifyC :: forall m b  a0 a1 a2 a3 a4 a5 a6 a7
          .  Functor m
          => f a0 a1 a2 a3 a4 a5 a6 a7
          -> Word
          -> (a0 -> m a0)          -> (a1 -> m a1)
          -> (a2 -> m a2)          -> (a3 -> m a3)
          -> (a4 -> m a4)          -> (a5 -> m a5)
          -> (a6 -> m a6)          -> (a7 -> m a7)
          -> m (f a0 a1 a2 a3 a4 a5 a6 a7)

instance Class (Functor8 f) (NonTerminal f) where cls = Sub Dict

type NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7 =
  ( NonTerminal f0, NonTerminal f1, NonTerminal f2, NonTerminal f3
  , NonTerminal f4, NonTerminal f5, NonTerminal f6, NonTerminal f7)

type RawEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r =
  StateT (Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n ann) m r
type RawEdit f0 f1 f2 f3 f4 f5 f6 f7  n ann r =
  RawEditT Identity f0 f1 f2 f3 f4 f5 f6 f7  n ann r

type MaybeRawEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r =
  RawEditT (MaybeT m)  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
type MaybeRawEdit  f0 f1 f2 f3 f4 f5 f6 f7  n ann r =
  MaybeRawEditT Identity  f0 f1 f2 f3 f4 f5 f6 f7  n ann r

modifyT :: Monad m => (s -> m s) -> StateT s m ()
modifyT f = put =<< lift . f =<< get

clearAnn :: ( Functor8 f0, Functor8 f1, Functor8 f2, Functor8 f3
            , Functor8 f4, Functor8 f5, Functor8 f6, Functor8 f7)
         => Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n ann
         -> Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n ()
clearAnn = mapAll $ const ()

diff :: Word -> Word -> Word
diff a b = (max a b) - (min a b)

justEdit :: Monad m
         => RawEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
         -> MaybeRawEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
justEdit = mapStateT (MaybeT . fmap Just)

maybeEdit' :: Monad m
           => MaybeRawEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
           -> RawEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann (Maybe r)
maybeEdit' try = do
  c <- get
  r <- lift $ runMaybeT $ runStateT try c
  case r of
    Nothing -> return Nothing
    Just (v, c') -> put c' >> return (Just v)

maybeEdit :: Monad m
          => MaybeRawEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
          -> RawEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
          -> RawEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
maybeEdit try catch = maybeEdit' try >>= \case
    Nothing -> catch
    Just v -> return v

tryEdit :: Monad m
        => MaybeRawEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
        -> RawEditT m  f0 f1 f2 f3 f4 f5 f6 f7 n ann ()
tryEdit = flip maybeEdit $ return ()

assumeMaybeEdit :: Monad m
                => RawEditT (MaybeT m)  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
                -> RawEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
assumeMaybeEdit = mapStateT $ fmap fromJust . runMaybeT

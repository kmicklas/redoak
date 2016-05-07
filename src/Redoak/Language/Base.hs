{-# LANGUAGE DeriveFunctor #-}
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

class ( NonTerminal f0, NonTerminal f1, NonTerminal f2, NonTerminal f3
      , NonTerminal f4, NonTerminal f5, NonTerminal f6, NonTerminal f7)
      => Language f0 f1 f2 f3 f4 f5 f6 f7 where

class Fresh a where
  fresh :: a -> a

instance Fresh Word where
  fresh = (+ 1)

getFresh :: (Fresh a, Monad m) => StateT a m a
getFresh = do
  i <- get
  put $ fresh i
  return i

clearAnn :: ( Functor8 f0, Functor8 f1, Functor8 f2, Functor8 f3
            , Functor8 f4, Functor8 f5, Functor8 f6, Functor8 f7)
         => Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n ann
         -> Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n ()
clearAnn = mapAll $ const ()

initAnn :: ( Traversable8 f0, Traversable8 f1, Traversable8 f2, Traversable8 f3
           , Traversable8 f4, Traversable8 f5, Traversable8 f6, Traversable8 f7)
        => (Fresh ann, Monad m)
        => Cofree8'  f0 f1 f2 f3 f4 f5 f6 f7  n old
        -> StateT ann m (Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n ann)
initAnn = traverseAll (const getFresh)

type Range n = (n, n)

data Tip n
  = Single n
  | Range (Range n)
  deriving (Eq, Ord, Show, Functor)

data Selection
  = Descend Word
  | Select (Tip Word)
  deriving (Eq, Ord, Show)

type Path = ([Word], Tip Word)

type Cursor f0 f1 f2 f3 f4 f5 f6 f7  n ann =
  Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n (ann, Selection)

type EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r =
  StateT (Cursor f0 f1 f2 f3 f4 f5 f6 f7  n ann) m r
type Edit f0 f1 f2 f3 f4 f5 f6 f7  n ann r =
  EditT Identity f0 f1 f2 f3 f4 f5 f6 f7  n ann r

type MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r =
  EditT (MaybeT m)  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
type MaybeEdit  f0 f1 f2 f3 f4 f5 f6 f7  n ann r =
  MaybeEditT Identity  f0 f1 f2 f3 f4 f5 f6 f7  n ann r

diff :: Word -> Word -> Word
diff a b = (max a b) - (min a b)

justEdit :: Monad m
         => EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
         -> MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
justEdit = mapStateT (MaybeT . fmap Just)

maybeEdit :: Monad m
          => MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
          -> EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
          -> EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
maybeEdit try catch = do
  c <- get
  r <- lift $ runMaybeT $ runStateT try c
  case r of
    Nothing -> catch
    Just (v, c') -> put c' >> return v

tryEdit :: Monad m
        => MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann ()
        -> EditT m  f0 f1 f2 f3 f4 f5 f6 f7 n ann ()
tryEdit = flip maybeEdit $ return ()

assumeMaybeEdit :: Monad m
                => EditT (MaybeT m)  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
                -> EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
assumeMaybeEdit = mapStateT $ fmap fromJust . runMaybeT

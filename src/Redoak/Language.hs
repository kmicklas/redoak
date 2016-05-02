{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Redoak.Language where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.Functor.Identity
import Data.Map
import Data.Maybe as M
import Data.Text hiding (index)

import Control.Comonad.Cofree8
import Data.Functor8
import Data.Traversable8


type Language f0 f1 f2 f3 f4 f5 f6 f7  a = Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  7  a

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
         => Language f0 f1 f2 f3 f4 f5 f6 f7 ann
         -> Language f0 f1 f2 f3 f4 f5 f6 f7 ()
clearAnn = mapAll $ const ()

initAnn :: ( Traversable8 f0, Traversable8 f1, Traversable8 f2, Traversable8 f3
           , Traversable8 f4, Traversable8 f5, Traversable8 f6, Traversable8 f7)
        => (Fresh ann, Monad m)
        => Language f0 f1 f2 f3 f4 f5 f6 f7 old
        -> StateT ann m (Language f0 f1 f2 f3 f4 f5 f6 f7 ann)
initAnn = traverseAll (const getFresh)

type Range n = (n, n)

data Tip
  = Single Word
  | Range (Range Word)
  deriving (Eq, Ord, Show)

data Selection
  = Descend Word
  | Select Tip
  deriving (Eq, Ord, Show)

type Path = ([Word], Tip)

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

class Functor8 f => NonTerminal f where
  -- | Number of children
  length :: forall a0 a1 a2 a3 a4 a5 a6 a7 . f a0 a1 a2 a3 a4 a5 a6 a7 -> Word

  -- | Introduction rules for auto-complete
  introductions :: Map Text (f () () () () () () () ())

  -- | Can we select a range of adjacent children or only a single child at a time?
  canSelectRange :: forall a0 a1 a2 a3 a4 a5 a6 a7 . f a0 a1 a2 a3 a4 a5 a6 a7 -> Bool

  index :: forall a . f a a a a a a a a -> Word -> a

path :: ( NonTerminal f0, NonTerminal f1, NonTerminal f2, NonTerminal f3
        , NonTerminal f4, NonTerminal f5, NonTerminal f6, NonTerminal f7)
     => Cursor  f0 f1 f2 f3 f4 f5 f6 f7 n ann -> Path
path = foldPoly (BlankDict :: BlankDict (NonTerminal Void8)) $ curry $ \case
  ((_, Descend i), nt) -> first (i :) $ index (foldFPoly path nt) i
  ((_, Select r),  _)  -> ([], r)


class ( NonTerminal f0, NonTerminal f1, NonTerminal f2, NonTerminal f3
      , NonTerminal f4, NonTerminal f5, NonTerminal f6, NonTerminal f7)
      => Langauge f0 f1 f2 f3 f4 f5 f6 f7 where

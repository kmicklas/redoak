{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Redoak.Language where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.Constraint
import Data.Functor.Identity
import Data.Map
import Data.Maybe as M
import Data.Proxy
import Data.Text hiding (index)
import Data.Type.Equality
import Data.Void

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

class Traversable8 f => NonTerminal f where
  -- | Number of children
  length :: forall a0 a1 a2 a3 a4 a5 a6 a7 . f a0 a1 a2 a3 a4 a5 a6 a7 -> Word

  -- | Introduction rules for auto-complete
  introductions :: Map Text (f () () () () () () () ())

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

index :: forall f a . NonTerminal f => f a a a a a a a a -> Word -> a
index nt i = runIdentity $ indexC nt i
  Identity Identity Identity Identity Identity Identity Identity Identity

path :: ( NonTerminal f0, NonTerminal f1, NonTerminal f2, NonTerminal f3
        , NonTerminal f4, NonTerminal f5, NonTerminal f6, NonTerminal f7)
     => Cursor  f0 f1 f2 f3 f4 f5 f6 f7 n ann -> Path
path l = foldPoly (Sub Dict :: forall a. NonTerminal a :- Functor8 a) (\x -> case (ann, x) of
    ((_, Descend i), nt) -> first (i :) $ index (foldFPoly path nt) i
    ((_, Select r),  _)  -> ([], r))
  l
  where ann = getAnn l

local :: forall m n ann r  f0 f1 f2 f3 f4 f5 f6 f7
      .  ( NonTerminal f0, NonTerminal f1, NonTerminal f2, NonTerminal f3
         , NonTerminal f4, NonTerminal f5, NonTerminal f6, NonTerminal f7
         , Monad m)
      => (forall n'. EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n' ann r)
      -> EditT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r
local f = do
  (a, sel) <- getAnn <$> get
  case sel of
    (Select _) -> f
    (Descend i) -> mapStatePoly (Sub Dict :: forall a. NonTerminal a :- Functor8 a) $ do
      nt <- get
      unless (canDescend nt) $ error "path is too deep" -- error not fail!
      let go :: forall n'
             .  Cursor f0 f1 f2 f3 f4 f5 f6 f7 n' ann
             -> PairT r m (Cursor f0 f1 f2 f3 f4 f5 f6 f7 n' ann)
          go x = PairT $ runStateT (local f) x
      (a, s) <- lift $ unPairT $ modifyC nt i go go go go go go go go
      put s
      return a

localMove :: ( NonTerminal f0, NonTerminal f1, NonTerminal f2, NonTerminal f3
             , NonTerminal f4, NonTerminal f5, NonTerminal f6, NonTerminal f7
             , Monad m)
          => (Int -> Tip Int -> Maybe (Tip Int))
          -> MaybeEditT m  f0 f1 f2 f3 f4 f5 f6 f7  a ann ()
localMove f = local $ do
  (a, Select tip) <- getAnn <$> get
  tip'' <- mapStatePoly (Sub Dict :: forall a. NonTerminal a :- Functor8 a) $ do
    nt <- get
    let len = fromIntegral $ Redoak.Language.length nt
    tip' <- lift $ MaybeT $ return $ f len $ fromIntegral <$> tip
    guard $ case tip' of
      Single p           -> p < len
      Range (start, end) -> min start end > 0 || max start end < len
    return tip'
  modify $ \e -> setAnn e (a, Select $ fromIntegral <$> tip'')


class ( NonTerminal f0, NonTerminal f1, NonTerminal f2, NonTerminal f3
      , NonTerminal f4, NonTerminal f5, NonTerminal f6, NonTerminal f7)
      => Langauge f0 f1 f2 f3 f4 f5 f6 f7 where

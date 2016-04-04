{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Redoak.Tree
  ( Cofree(..)
  , Tree
  , Trunk
  , Element(..)
  , Fresh

  , getFresh

  , elimIsSequence
  , mapIsSequence

  , initAnn
  , clearAnn
  ) where

import           Control.Comonad.Cofree
import           Control.Monad.Trans.State
import           Control.Lens hiding ((:<))
import           Control.Lens.TH
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bifunctor.TH
import           Data.Bitraversable
import           Data.Sequence hiding ((:<))
import           Data.Sequences as SS


data Element a b
  = Atom { _value :: a }
  | Node { _children :: Seq b }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
makeLenses ''Element
deriveBifunctor ''Element
deriveBifoldable ''Element
deriveBitraversable ''Element

type Tree a ann = Cofree (Element a) ann

-- | A Trunk is the unidentified part of a Tree
type Trunk a ann = Element a (Tree a ann)

newtype T f a ann = T { unT :: Cofree (f a) ann }
  deriving (
      --Eq, Ord, Show,
      Functor, Foldable, Traversable)

instance Bifunctor ff => Bifunctor (T ff) where
  bimap f g = T . go . unT where
    go (a :< e) = g a :< bimap f go e

instance Bifoldable ff => Bifoldable (T ff) where
  bifoldMap f g = go . unT where
    go (a :< e) = g a `mappend` bifoldMap f go e

instance {-Bitraversable ff => -}Bitraversable (T Element)  where
  bitraverse f g = fmap T . go . unT where
    go (a :< as) = (:<) <$> g a <*> bitraverse f go as

class Fresh a where
  fresh :: a -> a

instance Fresh Word where
  fresh = (+ 1)

getFresh :: (Fresh a, Monad m) => StateT a m a
getFresh = do
  i <- get
  put $ fresh i
  return i

elimIsSequence :: forall a n ret
               .  IsSequence a
               => (forall s. IsSequence s => s -> ret)
               -> Element a n
               -> ret
elimIsSequence f = \case
  Atom a -> f a
  Node s -> f s


mapIsSequence :: forall a n
              .  IsSequence a
              => (forall s. IsSequence s => s -> s)
              -> Element a n
              -> Element a n
mapIsSequence f = \case
  Atom a -> Atom $ f a
  Node s -> Node $ f s

clearAnn :: Tree a ann -> Tree a ()
clearAnn = fmap $ const ()

initAnn :: (Fresh ann, Monad m) => Tree a old -> StateT ann m (Tree a ann)
initAnn = fmap unT . bitraverse pure (const getFresh) . T

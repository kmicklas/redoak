{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
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

  , ann
  , getFresh

  , elimIsSequence
  , mapIsSequence

  , initAnn
  , clearAnn
  ) where

import           Control.Comonad.Cofree
import           Control.Monad.Trans.State
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bifunctor.TH
import           Data.Bitraversable
import           Data.Sequence hiding ((:<))
import           Data.Sequences as SS


data Element a b
  = Atom { value :: a }
  | Node { children :: Seq b }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
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

ann :: Tree a ann -> ann
ann (a :< _) = a

elimIsSequence :: forall seq x ret
               .  IsSequence seq
               => (forall s. IsSequence s => s -> ret)
               -> Element seq x
               -> ret
elimIsSequence f = \case
  Atom a -> f a
  Node s -> f s

mapIsSequence :: forall seq ann
              .  IsSequence seq
              => (forall s. IsSequence s => s -> s)
              -> Element seq ann
              -> Element seq ann
mapIsSequence f = \case
  Atom a -> Atom $ f a
  Node s -> Node $ f s

clearAnn :: Tree a ann -> Tree a ()
clearAnn = fmap $ const ()

initAnn :: (Fresh ann, Monad m) => Tree a old -> StateT ann m (Tree a ann)
initAnn = fmap unT . bitraverse pure (const getFresh) . T

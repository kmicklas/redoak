{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Redoak.Language.Fundamental where

import Control.Lens hiding ((:<))
import Control.Lens.TH
import Data.Bifoldable
import Data.Bifunctor
import Data.Bifunctor.TH
import Data.Bitraversable
import Data.Coerce
import Data.Sequence hiding ((:<))
import Data.Sequences as SS
import Data.Void

import Control.Comonad.Cofree8
import Data.Functor8


data Element a b
  = Atom { _value :: a }
  | Node { _children :: Seq b }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
makeLenses ''Element
deriveBifunctor ''Element
deriveBifoldable ''Element
deriveBitraversable ''Element


newtype LiftBf8 bf a a0 a1 a2 a3 a4 a5 a6 a7 = LiftBf8 { _lowerBf8 :: bf a a7 }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
makeLenses ''LiftBf8

instance Bifunctor f => Functor8 (LiftBf8 f a) where
  map8 _ _ _ _ _ _ _ f = lowerBf8 %~ second f


type MkBfTree bf a index ann =
  Cofree8'
    Void8 Void8 Void8 Void8 Void8 Void8 Void8 (LiftBf8 bf a)
    index
    ann

type MkTree a index ann = MkBfTree Element a index ann

type Tree a ann = MkTree a 7 ann

-- | A Trunk is the unidentified part of a Tree
type Trunk a ann =
  LiftBf8 Element a (MkTree a 0 ann) (MkTree a 1 ann) (MkTree a 2 ann) (MkTree a 3 ann)
                    (MkTree a 4 ann) (MkTree a 5 ann) (MkTree a 6 ann) (Tree a ann)

newtype Cofree8Bifunctor bf index a ann =
  Cofree8Bifunctor { unCofree8Bifunctor :: MkBfTree bf a index ann }

{-
instance Bifunctor bf => Bifunctor (Cofree8Bifunctor bf index) where
  bimap f g = Cofree8Bifunctor . _ . unCofree8Bifunctor where
    go = _ -- (a :< e) = g a :< bimap f go e

instance Bifoldable ff => Bifoldable (Cofree8Bifunctor ff) where
  bifoldMap f g = go . unCofree8Bifunctor where
    go (a :< e) = g a `mappend` bifoldMap f go e

instance Bitraversable ff => Bitraversable (Cofree8Bifunctor ff)  where
  bitraverse f g = fmap Cofree8Bifunctor . go . unCofree8Bifunctor where
    go (a :< as) = (:<) <$> g a <*> bitraverse f go as
-}

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

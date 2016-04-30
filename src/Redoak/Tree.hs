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
  , CoFreeBiFunctor(..)

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

import           Redoak.Language hiding (clearAnn)
import           Redoak.Language.Fundamental hiding (Tree, Trunk)

type Tree a ann = Cofree (Element a) ann

-- | A Trunk is the unidentified part of a Tree
type Trunk a ann = Element a (Tree a ann)

newtype CoFreeBiFunctor f a ann = CoFreeBiFunctor { unCoFreeBiFunctor :: Cofree (f a) ann }
  deriving (
      --Eq, Ord, Show,
      Functor, Foldable, Traversable)

instance Bifunctor ff => Bifunctor (CoFreeBiFunctor ff) where
  bimap f g = CoFreeBiFunctor . go . unCoFreeBiFunctor where
    go (a :< e) = g a :< bimap f go e

instance Bifoldable ff => Bifoldable (CoFreeBiFunctor ff) where
  bifoldMap f g = go . unCoFreeBiFunctor where
    go (a :< e) = g a `mappend` bifoldMap f go e

instance Bitraversable ff => Bitraversable (CoFreeBiFunctor ff)  where
  bitraverse f g = fmap CoFreeBiFunctor . go . unCoFreeBiFunctor where
    go (a :< as) = (:<) <$> g a <*> bitraverse f go as

clearAnn :: Tree a ann -> Tree a ()
clearAnn = fmap $ const ()

initAnn :: (Fresh ann, Monad m) => Tree a old -> StateT ann m (Tree a ann)
initAnn = fmap unCoFreeBiFunctor . bitraverse pure (const getFresh) . CoFreeBiFunctor

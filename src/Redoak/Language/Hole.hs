{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Redoak.Language.Hole where

import Prelude hiding (length)

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.Constraint
import Data.Functor.Identity
import Data.Map
import Data.Text (Text)

import Control.Comonad.Cofree8
import Data.Functor8
import Data.Foldable8
import Data.Traversable8

import Redoak.Language.Base


class NonTerminal f => Completable f where
  -- | Introduction rules for auto-complete
  introductions :: Map Text (f () () () () () () () ())

data WithHole f a0 a1 a2 a3 a4 a5 a6 a7
  = Filled   (f a0 a1 a2 a3 a4 a5 a6 a7)
  | Unfilled
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Functor8 f => Functor8 (WithHole f) where
  map8 f0 f1 f2 f3 f4 f5 f6 f7 = \case
    Filled e -> Filled $ map8 f0 f1 f2 f3 f4 f5 f6 f7 e
    Unfilled -> Unfilled

instance Foldable8 f => Foldable8 (WithHole f) where
  foldMap8 f0 f1 f2 f3 f4 f5 f6 f7 = \case
    Filled e -> foldMap8 f0 f1 f2 f3 f4 f5 f6 f7 e
    Unfilled -> mempty

instance Traversable8 f => Traversable8 (WithHole f) where
  traverse8 f0 f1 f2 f3 f4 f5 f6 f7 = \case
    Filled e -> Filled <$> traverse8 f0 f1 f2 f3 f4 f5 f6 f7 e
    Unfilled -> pure Unfilled


instance NonTerminal f => NonTerminal (WithHole f) where
  length = \case
    (Filled e) -> length e
    Unfilled   -> 0

  canSelectRange = \case
    (Filled e) -> canSelectRange e
    Unfilled   -> False

  canDescend = \case
    (Filled e) -> canDescend e
    Unfilled   -> False

  indexC e i = case e of
    (Filled e) -> indexC e i
    Unfilled   -> \ _ _ _ _ _ _ _ _ -> undefined

  modifyC e i f0 f1 f2 f3 f4 f5 f6 f7 = case e of
    (Filled e) -> Filled <$> modifyC e i f0 f1 f2 f3 f4 f5 f6 f7
    Unfilled   -> undefined -- could define with Applicative and pure,
                            -- but no valid index in this case anyways

instance Completable f => Completable (WithHole f) where
  introductions = Filled <$> introductions

--instance NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7
--         => Language (WithHole f0) (WithHole f1) (WithHole f2) (WithHole f3)
--                     (WithHole f4) (WithHole f5) (WithHole f6) (WithHole f7) where

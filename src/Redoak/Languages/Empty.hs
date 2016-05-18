{-# LANGUAGE EmptyCase #-}
module Redoak.Languages.Empty where

import Data.Map (empty)

import Control.Comonad.Cofree8
import Data.Functor8

import Redoak.Language
import Redoak.Language.Hole


instance NonTerminal Void8 where
  length         = \case {}
  canSelectRange = \case {}
  canDescend     = \case {}
  indexC         = \case {}
  modifyC        = \case {}

instance Completable Void8 where
  identifiers _ = Nothing
  introductions = Data.Map.empty

{-# LANGUAGE EmptyCase #-}
module Redoak.Languages.Empty where

import Data.Map (empty)

import Control.Comonad.Cofree8
import Data.Functor8

import Redoak.Language


instance NonTerminal Void8 where
  length         = \case {}
  canSelectRange = \case {}
  canDescend     = \case {}
  indexC         = \case {}
  modifyC        = \case {}

  introductions = Data.Map.empty


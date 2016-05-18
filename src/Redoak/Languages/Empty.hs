{-# LANGUAGE EmptyCase #-}
module Redoak.Languages.Empty where

import Control.Comonad.Cofree8
import Data.Functor8

import Redoak.Language


instance NonTerminal Void8 where
  length         = \case {}
  canSelectRange = \case {}
  canDescend     = \case {}
  indexC         = \case {}
  modifyC        = \case {}

{-# LANGUAGE TypeFamilies #-}
module Redoak.Layout.Identity where

import           Control.Monad.Identity
import qualified Data.Text as T

import           Redoak.Layout
import           Redoak.Rectangle

instance Rules Identity where
  type BaseNum Identity = Int

  inlineText t = return (W $ T.length t, maxInlineHeight)

instance Adequate Int where
  indentWidth     = W 8
  maxInlineHeight = H 1
  inlinePad       = W 0

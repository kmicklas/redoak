{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Redoak.Editor where

import           Control.Monad
import           Control.Monad.Identity

import           Data.Bifunctor
import           Data.Foldable
import           Data.List.NonEmpty
import           Data.Maybe
import           Data.Sequence hiding ((:<))
import           Data.Sequences (IsSequence)
import           Data.Text (Text)
import           Reflex
import           Reflex.Dom

import           Redoak.Event
import           Redoak.Language
import           Redoak.Language.Hole () -- to make sure it compiles
import qualified Redoak.Languages.Fundamental as Fundamental
import qualified Redoak.Languages.C as C


--- Non-parametric existential, basically
-- TODO more langs
data Multiplexed
 = Fundamental Fundamental.AccumP
 | C           C.AccumP

initState = Fundamental Fundamental.initState

handleEvent' :: KeyEvent -> Multiplexed -> Multiplexed
handleEvent' e = \case
  (Fundamental a) -> Fundamental $ Redoak.Language.handleEvent e a
  (C a)           -> C $ Redoak.Language.handleEvent e a


handleEvents :: NonEmpty KeyEvent -> Multiplexed -> Multiplexed
handleEvents es a = foldl' (flip handleEvent') a es

splitMultiplexed :: MonadWidget t m
                 => Multiplexed
                 -> (Fundamental.Term', Text, m ())
splitMultiplexed = \case
  (Fundamental a) -> (fst a, t, w)
    where (t, w) = getMessage a
  (C a)           -> (C.conv a, t, w)
    where (t, w) = getMessage a

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


class ( Language f0 f1 f2 f3 f4 f5 f6 f7
      , Language g0 g1 g2 g3 g4 g5 g6 g7)
      => Renderable  f0 f1 f2 f3 f4 f5 f6 f7  g0 g1 g2 g3 g4 g5 g6 g7 where

  convert :: ( RootTerm  f0 f1 f2 f3 f4 f5 f6 f7
             , Accum     f0 f1 f2 f3 f4 f5 f6 f7)
          -> ( RootTerm  f0 f1 f2 f3 f4 f5 f6 f7
             , Accum     f0 f1 f2 f3 f4 f5 f6 f7)

instance Language f0 f1 f2 f3 f4 f5 f6 f7
         => Renderable  f0 f1 f2 f3 f4 f5 f6 f7  f0 f1 f2 f3 f4 f5 f6 f7 where

  convert = id

--- Non-parametric existential, basically
-- TODO more langs
data Multiplexed
 = Fundamental Fundamental.Accum''

initState = Fundamental Fundamental.initState

handleEvent' :: KeyEvent -> Multiplexed -> Multiplexed
handleEvent' e = \case
  (Fundamental a) -> Fundamental $ Redoak.Language.handleEvent e a

handleEvents :: NonEmpty KeyEvent -> Multiplexed -> Multiplexed
handleEvents es a = foldl' (flip handleEvent') a es

splitMultiplexed :: MonadWidget t m
                 => Multiplexed
                 -> (Fundamental.Term', Text, m ())
splitMultiplexed = \case
  (Fundamental a) -> (fst a, t, w)
    where (t, w) = getMessage a

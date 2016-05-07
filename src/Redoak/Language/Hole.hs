{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Redoak.Language.Hole where

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

import Redoak.Language.Base


class NonTerminal f => Completable f where
  -- | Introduction rules for auto-complete
  introductions :: Map Text (f () () () () () () () ())


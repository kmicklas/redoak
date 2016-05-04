{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Redoak.Tree.Range
  ( Range
  , Selection(..)
  , Path
  , Cursor

  , EditT
  , Edit
  , MaybeEditT
  , MaybeEdit

  , justEdit
  , maybeEdit
  , tryEdit
  , getFresh
  , path

  , elimIsSequence
  , mapIsSequence

  , initAnn
  , clearAnn
  , unCursor
  , initCursor
  , getSelection
  , isEmpty
  , isInAtom

  , delete
  , change
  , insertNode
  , Redoak.Language.Fundamental.reverse
  , ascend
  , descend
  , wrap
  , Redoak.Language.Fundamental.unwrap
  , push
  , pop

  , switchBounds
  , startMin
  , endMax
  , selectAll
  , selectNoneStart
  , selectNoneEnd
  , shiftLeft
  , shiftRight
  , moveLeft
  , moveRight
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Functor.Identity
import           Data.Maybe as M
import           Data.MonoTraversable hiding (Element)
import           Data.Monoid
import qualified Data.Sequence as S
import           Data.Sequence hiding ((:<))
import           Data.Sequences as SS
import           Data.Word

import           Control.Comonad.Cofree8

import           Redoak.Language
import           Redoak.Language.Fundamental

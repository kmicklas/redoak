{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
module Redoak.Language.Fresh
  ( Fresh(..)
  , FreshT
  , runFreshT
  , initAnn
  , getFresh
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Constraint
import Data.Functor.Identity
import Data.Maybe as M

import Control.Comonad.Cofree8
import Data.Functor8
import Data.Traversable8

import Redoak.Language.Base


class Fresh a where
  fresh :: a -> a

instance Fresh Word where
  fresh = (+ 1)

newtype FreshT s m a = FreshT (StateT s m a)
 deriving (Monad, Functor, Applicative, MonadTrans)

runFreshT :: FreshT s m a -> s -> m (a, s)
runFreshT (FreshT s) = runStateT s

getFresh :: (Fresh a, Monad m) => FreshT a m a
getFresh = FreshT $ do
  i <- get
  put $ fresh i
  return i

initAnn :: ( Traversable8 f0, Traversable8 f1, Traversable8 f2, Traversable8 f3
           , Traversable8 f4, Traversable8 f5, Traversable8 f6, Traversable8 f7)
        => (Fresh ann, Monad m)
        => Cofree8'  f0 f1 f2 f3 f4 f5 f6 f7  n old
        -> FreshT ann m (Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n ann)
initAnn = traverseAll (const getFresh)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Redoak.Language where

import Control.Monad.Trans.State

import Control.Comonad.Cofree8
import Data.Functor8
import Data.Traversable8

type Language f0 f1 f2 f3 f4 f5 f6 f7  a = Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  7  a

class Fresh a where
  fresh :: a -> a

instance Fresh Word where
  fresh = (+ 1)

getFresh :: (Fresh a, Monad m) => StateT a m a
getFresh = do
  i <- get
  put $ fresh i
  return i

clearAnn :: ( Functor8 f0, Functor8 f1, Functor8 f2, Functor8 f3
            , Functor8 f4, Functor8 f5, Functor8 f6, Functor8 f7)
         => Language f0 f1 f2 f3 f4 f5 f6 f7 ann
         -> Language f0 f1 f2 f3 f4 f5 f6 f7 ()
clearAnn = mapAll $ const ()

initAnn :: ( Traversable8 f0, Traversable8 f1, Traversable8 f2, Traversable8 f3
           , Traversable8 f4, Traversable8 f5, Traversable8 f6, Traversable8 f7)
        => (Fresh ann, Monad m)
        => Language f0 f1 f2 f3 f4 f5 f6 f7 old
        -> StateT ann m (Language f0 f1 f2 f3 f4 f5 f6 f7 ann)
initAnn = traverseAll (const getFresh)

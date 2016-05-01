{-# LANGUAGE EmptyCase #-}
module Data.Traversable8 where

import Control.Applicative
import Data.Coerce
--import Control.Monad.Trans.Instances ()
--import Data.Functor.Constant
--import Data.Orphans ()

import Data.Functor8
import Data.Foldable8


class (Functor8 t, Foldable8 t) => Traversable8 t where
  -- | Evaluates the relevant functions at each element in the structure, running
  -- the action, and builds a new structure with the same shape, using the
  -- elements produced from sequencing the actions.
  --
  -- @'traverse8' f g â‰¡ 'sequence8A' . 'bimap' f g@
  traverse8 :: Applicative f
             => (a0 -> f b0) -> (a1 -> f b1) -> (a2 -> f b2) -> (a3 -> f b3)
             -> (a4 -> f b4) -> (a5 -> f b5) -> (a6 -> f b6) -> (a7 -> f b7)
             -> t a0 a1 a2 a3 a4 a5 a6 a7 -> f (t b0 b1 b2 b3 b4 b5 b6 b7)

-- | Sequences all the actions in a structure, building a new structure with the
-- same shape using the results of the actions.
--
-- @'sequence8A' â‰¡ 'traverse8' 'id' 'id'@
sequence8A :: (Traversable8 t, Applicative f)
           => t (f a0) (f a1) (f a2) (f a3) (f a4) (f a5) (f a6) (f a7)
           -> f (t a0 a1 a2 a3 a4 a5 a6 a7)
sequence8A = traverse8 id id id id id id id id
{-# INLINE sequence8A #-}

-- | As 'traverse8', but uses evidence that @m@ is a 'Monad' rather than an
-- 'Applicative'.
--
-- @
-- 'mapM8' f g â‰¡ 'sequence8' . 'bimap' f g
-- 'mapM8' f g â‰¡ 'unwrapMonad' . 'traverse8' ('WrapMonad' . f) ('WrapMonad' . g)
-- @
mapM8 :: (Traversable8 t, Monad m)
      => (a0 -> m b0) -> (a1 -> m b1) -> (a2 -> m b2) -> (a3 -> m b3)
      -> (a4 -> m b4) -> (a5 -> m b5) -> (a6 -> m b6) -> (a7 -> m b7)
      -> t a0 a1 a2 a3 a4 a5 a6 a7 -> m (t b0 b1 b2 b3 b4 b5 b6 b7)
mapM8 f0 f1 f2 f3 f4 f5 f6 f7 = unwrapMonad . traverse8
  (WrapMonad . f0) (WrapMonad . f1) (WrapMonad . f2) (WrapMonad . f3)
  (WrapMonad . f4) (WrapMonad . f5) (WrapMonad . f6) (WrapMonad . f7)
{-# INLINE mapM8 #-}

-- | As 'sequence8A', but uses evidence that @m@ is a 'Monad' rather than an
-- 'Applicative'.
--
-- @
-- 'sequence8' â‰¡ 'mapM8' 'id' 'id'
-- 'sequence8' â‰¡ 'unwrapMonad' . 'sequence8A' . 'bimap' 'WrapMonad' 'WrapMonad'
-- @
sequence8 :: (Traversable8 t, Monad m)
          => t (m a0) (m a1) (m a2) (m a3) (m a4) (m a5) (m a6) (m a7)
          -> m (t a0 a1 a2 a3 a4 a5 a6 a7)
sequence8 = mapM8 id id id id id id id id
{-# INLINE sequence8 #-}


instance Traversable8 (Const8 x) where
  traverse8 _ _ _ _ _ _ _ _ = pure . coerce


instance Traversable8 Void8 where
  traverse8 _ _ _ _ _ _ _ _ x = case x of { }

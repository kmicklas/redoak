{-# LANGUAGE EmptyCase #-}
module Data.Foldable8 where

import Control.Applicative
import Control.Monad
import Data.Functor.Constant
import Data.Maybe (fromMaybe)
import Data.Monoid

import Data.Functor8

class Foldable8 p where
  -- | Combines the elements of a structure using a monoid.
  --
  -- @'fold8' â‰¡ 'foldMap8' 'id' 'id'@
  fold8 :: Monoid m => p m m m m m m m m -> m
  fold8 = foldMap8 id id id id id id id id
  {-# INLINE fold8 #-}

  -- | Combines the elements of a structure, given ways of mapping them to a
  -- common monoid.
  --
  -- @'foldMap8' f g â‰¡ 'foldf8' ('mappend' . f) ('mappend' . g) 'mempty'@
  foldMap8 :: Monoid m
           => (a0 -> m) -> (a1 -> m)
           -> (a2 -> m) -> (a3 -> m)
           -> (a4 -> m) -> (a5 -> m)
           -> (a6 -> m) -> (a7 -> m)
           -> p a0 a1 a2 a3 a4 a5 a6 a7 -> m
  foldMap8 f0 f1 f2 f3 f4 f5 f6 f7 = foldr8
    (mappend . f0) (mappend . f1) (mappend . f2) (mappend . f3)
    (mappend . f4) (mappend . f5) (mappend . f6) (mappend . f7)
    mempty
  {-# INLINE foldMap8 #-}

  -- | Combines the elements of a structure in a right associative manner. Given
  -- a hypothetical function @toEitherList :: p a b -> [Either a b]@ yielding a
  -- list of all elements of a structure in order, the following would hold:
  --
  -- @'foldf8' f g z â‰¡ 'foldr' ('either' f g) z . toEitherList@
  foldr8 :: (a0 -> c -> c) -> (a1 -> c -> c)
         -> (a2 -> c -> c) -> (a3 -> c -> c)
         -> (a4 -> c -> c) -> (a5 -> c -> c)
         -> (a6 -> c -> c) -> (a7 -> c -> c)
         -> c -> p a0 a1 a2 a3 a4 a5 a6 a7 -> c
  foldr8 f0 f1 f2 f3 f4 f5 f6 f7 z t = appEndo (go t) z where
    go = foldMap8
      (Endo . f0) (Endo . f1) (Endo . f2) (Endo . f3)
      (Endo . f4) (Endo . f5) (Endo . f6) (Endo . f7)
  {-# INLINE foldr8 #-}

  -- | Combines the elments of a structure in a left associative manner. Given a
  -- hypothetical function @toEitherList :: p a b -> [Either a b]@ yielding a
  -- list of all elements of a structure in order, the following would hold:
  --
  -- @'foldl8' f g z â‰¡ 'foldl' (\acc -> 'either' (f acc) (g acc)) z .  toEitherList@
  foldl8 :: (c -> a0 -> c) -> (c -> a1 -> c)
         -> (c -> a2 -> c) -> (c -> a3 -> c)
         -> (c -> a4 -> c) -> (c -> a5 -> c)
         -> (c -> a6 -> c) -> (c -> a7 -> c)
         -> c -> p a0 a1 a2 a3 a4 a5 a6 a7 -> c
  foldl8 f0 f1 f2 f3 f4 f5 f6 f7 z t = appEndo (getDual $ go t) z where
    go = foldMap8
      (Dual . Endo . flip f0) (Dual . Endo . flip f1)
      (Dual . Endo . flip f2) (Dual . Endo . flip f3)
      (Dual . Endo . flip f4) (Dual . Endo . flip f5)
      (Dual . Endo . flip f6) (Dual . Endo . flip f7)
  {-# INLINE foldl8 #-}


instance Foldable8 (Const8 x) where
  foldMap8 _ _ _ _ _ _ _ _ = mempty


instance Foldable8 Void8 where
  foldMap8 _ _ _ _ _ _ _ _ x = case x of { }

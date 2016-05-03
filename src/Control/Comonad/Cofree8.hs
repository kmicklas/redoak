{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
-- TODO Generalize 8 to N
module Control.Comonad.Cofree8 where

import Control.Comonad
import Control.Lens
import Control.Lens.TH
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.Constraint
import Data.Constraint.Lifting
import Data.Monoid
import GHC.TypeLits

import Data.Functor8
import Data.Foldable8
import Data.Traversable8

type Cofree8Inner f  f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7
  = (f (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  0  a0 a1 a2 a3 a4 a5 a6 a7)
       (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  1  a0 a1 a2 a3 a4 a5 a6 a7)
       (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  2  a0 a1 a2 a3 a4 a5 a6 a7)
       (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  3  a0 a1 a2 a3 a4 a5 a6 a7)
       (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  4  a0 a1 a2 a3 a4 a5 a6 a7)
       (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  5  a0 a1 a2 a3 a4 a5 a6 a7)
       (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  6  a0 a1 a2 a3 a4 a5 a6 a7)
       (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  7  a0 a1 a2 a3 a4 a5 a6 a7))

data Cofree8
       f0 f1 f2 f3 f4 f5 f6 f7
       (n :: Nat)
       a0 a1 a2 a3 a4 a5 a6 a7
       :: * where

  CF0 :: a0
      -> Cofree8Inner f0  f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  0  a0 a1 a2 a3 a4 a5 a6 a7

  CF1 :: a1
      -> Cofree8Inner f1  f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  1  a0 a1 a2 a3 a4 a5 a6 a7

  CF2 :: a2
      -> Cofree8Inner f2  f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  2  a0 a1 a2 a3 a4 a5 a6 a7

  CF3 :: a3
      -> Cofree8Inner f3  f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  3  a0 a1 a2 a3 a4 a5 a6 a7

  CF4 :: a4
      -> Cofree8Inner f4  f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  4  a0 a1 a2 a3 a4 a5 a6 a7

  CF5 :: a5
      -> Cofree8Inner f5  f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  5  a0 a1 a2 a3 a4 a5 a6 a7

  CF6 :: a6
      -> Cofree8Inner f6  f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  6  a0 a1 a2 a3 a4 a5 a6 a7

  CF7 :: a7
      -> Cofree8Inner f7  f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  7  a0 a1 a2 a3 a4 a5 a6 a7


mapFPoly :: ( Functor8 f0, Functor8 f1, Functor8 f2, Functor8 f3
            , Functor8 f4, Functor8 f5, Functor8 f6, Functor8 f7
            , Functor8 f)
         => (forall n
             .  Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  n  a0 a1 a2 a3 a4 a5 a6 a7
             -> Cofree8 g0 g1 g2 g3 g4 g5 g6 g7  n  b0 b1 b2 b3 b4 b5 b6 b7)
         -> Cofree8Inner f  f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7
         -> Cofree8Inner f  g0 g1 g2 g3 g4 g5 g6 g7  b0 b1 b2 b3 b4 b5 b6 b7
mapFPoly f = map8 f f f f f f f f

foldFPoly :: ( Functor8 f0, Functor8 f1, Functor8 f2, Functor8 f3
             , Functor8 f4, Functor8 f5, Functor8 f6, Functor8 f7
             , Functor8 f)
          => (forall n
              .  Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  n  a0 a1 a2 a3 a4 a5 a6 a7
              -> r)
          -> Cofree8Inner f  f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7
          -> f r r r r r r r r
foldFPoly f = map8 f f f f f f f f

traverseFPoly :: ( Functor8 f0, Functor8 f1, Functor8 f2, Functor8 f3
                 , Functor8 f4, Functor8 f5, Functor8 f6, Functor8 f7
                 , Functor8 f)
              => (forall n
                  .  Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  n  a0 a1 a2 a3 a4 a5 a6 a7
                  -> g (Cofree8 g0 g1 g2 g3 g4 g5 g6 g7  n  b0 b1 b2 b3 b4 b5 b6 b7))
              -> Cofree8Inner f  f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7
              -> f (g (Cofree8 g0 g1 g2 g3 g4 g5 g6 g7  0  b0 b1 b2 b3 b4 b5 b6 b7))
                   (g (Cofree8 g0 g1 g2 g3 g4 g5 g6 g7  1  b0 b1 b2 b3 b4 b5 b6 b7))
                   (g (Cofree8 g0 g1 g2 g3 g4 g5 g6 g7  2  b0 b1 b2 b3 b4 b5 b6 b7))
                   (g (Cofree8 g0 g1 g2 g3 g4 g5 g6 g7  3  b0 b1 b2 b3 b4 b5 b6 b7))
                   (g (Cofree8 g0 g1 g2 g3 g4 g5 g6 g7  4  b0 b1 b2 b3 b4 b5 b6 b7))
                   (g (Cofree8 g0 g1 g2 g3 g4 g5 g6 g7  5  b0 b1 b2 b3 b4 b5 b6 b7))
                   (g (Cofree8 g0 g1 g2 g3 g4 g5 g6 g7  6  b0 b1 b2 b3 b4 b5 b6 b7))
                   (g (Cofree8 g0 g1 g2 g3 g4 g5 g6 g7  7  b0 b1 b2 b3 b4 b5 b6 b7))
traverseFPoly f = map8 f f f f f f f f


instance forall f0 f1 f2 f3 f4 f5 f6 f7 n
         . ( Functor8 f0, Functor8 f1, Functor8 f2, Functor8 f3
           , Functor8 f4, Functor8 f5, Functor8 f6, Functor8 f7)
         => Functor8 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7 n) where
  map8 :: forall a0 a1 a2 a3 a4 a5 a6 a7
                 b0 b1 b2 b3 b4 b5 b6 b7
       .  (a0 -> b0) -> (a1 -> b1) -> (a2 -> b2) -> (a3 -> b3)
       -> (a4 -> b4) -> (a5 -> b5) -> (a6 -> b6) -> (a7 -> b7)
       -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  n  a0 a1 a2 a3 a4 a5 a6 a7
       -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  n  b0 b1 b2 b3 b4 b5 b6 b7
  map8 f0 f1 f2 f3 f4 f5 f6 f7 = go where
    go :: forall n'
       .  Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  n'  a0 a1 a2 a3 a4 a5 a6 a7
       -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  n'  b0 b1 b2 b3 b4 b5 b6 b7
    go = \case
      CF0 a r -> CF0 (f0 a) (mapFPoly go r)
      CF1 a r -> CF1 (f1 a) (mapFPoly go r)
      CF2 a r -> CF2 (f2 a) (mapFPoly go r)
      CF3 a r -> CF3 (f3 a) (mapFPoly go r)
      CF4 a r -> CF4 (f4 a) (mapFPoly go r)
      CF5 a r -> CF5 (f5 a) (mapFPoly go r)
      CF6 a r -> CF6 (f6 a) (mapFPoly go r)
      CF7 a r -> CF7 (f7 a) (mapFPoly go r)


instance forall f0 f1 f2 f3 f4 f5 f6 f7 n
         . ( Foldable8 f0, Foldable8 f1, Foldable8 f2, Foldable8 f3
           , Foldable8 f4, Foldable8 f5, Foldable8 f6, Foldable8 f7)
         => Foldable8 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7 n) where
  foldMap8 :: forall a0 a1 a2 a3 a4 a5 a6 a7
                     m
           .  Monoid m
           => (a0 -> m) -> (a1 -> m) -> (a2 -> m) -> (a3 -> m)
           -> (a4 -> m) -> (a5 -> m) -> (a6 -> m) -> (a7 -> m)
           -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  n  a0 a1 a2 a3 a4 a5 a6 a7
           -> m
  foldMap8 f0 f1 f2 f3 f4 f5 f6 f7 = go where
    go :: forall n'
       .  Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  n'  a0 a1 a2 a3 a4 a5 a6 a7
       -> m
    go = \case
      CF0 a r -> f0 a <> foldMap8 go go go go go go go go r -- fold8 (foldFPoly go r)
      CF1 a r -> f1 a <> foldMap8 go go go go go go go go r
      CF2 a r -> f2 a <> foldMap8 go go go go go go go go r
      CF3 a r -> f3 a <> foldMap8 go go go go go go go go r
      CF4 a r -> f4 a <> foldMap8 go go go go go go go go r
      CF5 a r -> f5 a <> foldMap8 go go go go go go go go r
      CF6 a r -> f6 a <> foldMap8 go go go go go go go go r
      CF7 a r -> f7 a <> foldMap8 go go go go go go go go r


instance forall f0 f1 f2 f3 f4 f5 f6 f7 n
         . ( Traversable8 f0, Traversable8 f1, Traversable8 f2, Traversable8 f3
           , Traversable8 f4, Traversable8 f5, Traversable8 f6, Traversable8 f7)
         => Traversable8 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7 n) where
  traverse8 :: forall a0 a1 a2 a3 a4 a5 a6 a7
                      b0 b1 b2 b3 b4 b5 b6 b7
                      f
            .  Applicative f
            => (a0 -> f b0) -> (a1 -> f b1) -> (a2 -> f b2) -> (a3 -> f b3)
            -> (a4 -> f b4) -> (a5 -> f b5) -> (a6 -> f b6) -> (a7 -> f b7)
            -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  n  a0 a1 a2 a3 a4 a5 a6 a7
            -> f (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  n  b0 b1 b2 b3 b4 b5 b6 b7)
  traverse8 f0 f1 f2 f3 f4 f5 f6 f7 = go where
    go :: forall n'
       .  Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  n'  a0 a1 a2 a3 a4 a5 a6 a7
       -> f (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  n'  b0 b1 b2 b3 b4 b5 b6 b7)
    go = \case
      CF0 a r -> CF0 <$> f0 a <*> sequence8A (traverseFPoly go r)
      CF1 a r -> CF1 <$> f1 a <*> sequence8A (traverseFPoly go r)
      CF2 a r -> CF2 <$> f2 a <*> sequence8A (traverseFPoly go r)
      CF3 a r -> CF3 <$> f3 a <*> sequence8A (traverseFPoly go r)
      CF4 a r -> CF4 <$> f4 a <*> sequence8A (traverseFPoly go r)
      CF5 a r -> CF5 <$> f5 a <*> sequence8A (traverseFPoly go r)
      CF6 a r -> CF6 <$> f6 a <*> sequence8A (traverseFPoly go r)
      CF7 a r -> CF7 <$> f7 a <*> sequence8A (traverseFPoly go r)


type Cofree8' f0 f1 f2 f3 f4 f5 f6 f7   n  a =
  Cofree8 f0 f1 f2 f3 f4 f5 f6 f7
          n
          a  a  a  a  a  a  a  a

type Cofree8Inner' f  f0 f1 f2 f3 f4 f5 f6 f7  a
  = (f (Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  0  a)
       (Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  1  a)
       (Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  2  a)
       (Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  3  a)
       (Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  4  a)
       (Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  5  a)
       (Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  6  a)
       (Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  7  a))

-- The dict helps with inference

getAnn :: Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n  a -> a
getAnn = \case
  CF0 a r -> a
  CF1 a r -> a
  CF2 a r -> a
  CF3 a r -> a
  CF4 a r -> a
  CF5 a r -> a
  CF6 a r -> a
  CF7 a r -> a

setAnn :: Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n  a
       -> a
       -> Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n  a
setAnn e a = case e of
  CF0 _ r -> CF0 a r
  CF1 _ r -> CF1 a r
  CF2 _ r -> CF2 a r
  CF3 _ r -> CF3 a r
  CF4 _ r -> CF4 a r
  CF5 _ r -> CF5 a r
  CF6 _ r -> CF6 a r
  CF7 _ r -> CF7 a r

modifyAnn :: (a -> a)
          -> Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n  a
          -> Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n  a
modifyAnn f e = setAnn e $ f $ getAnn e

mapPoly :: forall f0 f1 f2 f3 f4 f5 f6 f7
                  a k n
        .  ( k f0, k f1, k f2, k f3
           , k f4, k f5, k f6, k f7)
        => (forall f . k f :- Functor8 f)
        -> (forall f
            .  (Functor8 f, k f)
            => Cofree8Inner' f  f0 f1 f2 f3 f4 f5 f6 f7 a
            -> Cofree8Inner' f  f0 f1 f2 f3 f4 f5 f6 f7 a)
        -> Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n  a
        -> Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n  a
mapPoly prf f = \case
  CF0 a r -> CF0 a $ f r \\ (prf :: k f0 :- Functor8 f0)
  CF1 a r -> CF1 a $ f r \\ (prf :: k f1 :- Functor8 f1)
  CF2 a r -> CF2 a $ f r \\ (prf :: k f2 :- Functor8 f2)
  CF3 a r -> CF3 a $ f r \\ (prf :: k f3 :- Functor8 f3)
  CF4 a r -> CF4 a $ f r \\ (prf :: k f4 :- Functor8 f4)
  CF5 a r -> CF5 a $ f r \\ (prf :: k f5 :- Functor8 f5)
  CF6 a r -> CF6 a $ f r \\ (prf :: k f6 :- Functor8 f6)
  CF7 a r -> CF7 a $ f r \\ (prf :: k f7 :- Functor8 f7)

mapPolyF :: forall f0 f1 f2 f3 f4 f5 f6 f7
                   a k n m r
         .  ( k f0, k f1, k f2, k f3
            , k f4, k f5, k f6, k f7
            , Functor m)
         => (forall f . k f :- Functor8 f)
         -> (forall f
             .  (Functor8 f, k f)
             => Cofree8Inner' f  f0 f1 f2 f3 f4 f5 f6 f7 a
             -> m (Cofree8Inner' f  f0 f1 f2 f3 f4 f5 f6 f7 a))
         -> Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n  a
         -> m (Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n  a)
mapPolyF prf f = \case
  CF0 a r -> (CF0 a) <$> (f r \\ (prf :: k f0 :- Functor8 f0))
  CF1 a r -> (CF1 a) <$> (f r \\ (prf :: k f1 :- Functor8 f1))
  CF2 a r -> (CF2 a) <$> (f r \\ (prf :: k f2 :- Functor8 f2))
  CF3 a r -> (CF3 a) <$> (f r \\ (prf :: k f3 :- Functor8 f3))
  CF4 a r -> (CF4 a) <$> (f r \\ (prf :: k f4 :- Functor8 f4))
  CF5 a r -> (CF5 a) <$> (f r \\ (prf :: k f5 :- Functor8 f5))
  CF6 a r -> (CF6 a) <$> (f r \\ (prf :: k f6 :- Functor8 f6))
  CF7 a r -> (CF7 a) <$> (f r \\ (prf :: k f7 :- Functor8 f7))


data PairT a m b = PairT { unPairT :: (m (a, b)) }
                 deriving (Functor)

mapStatePoly :: forall f0 f1 f2 f3 f4 f5 f6 f7
                       a k n m r
             .  ( k f0, k f1, k f2, k f3
                , k f4, k f5, k f6, k f7
                , Monad m)
             => (forall f . k f :- Functor8 f)
             -> (forall f
                 .  (Functor8 f, k f)
                 => StateT (Cofree8Inner' f  f0 f1 f2 f3 f4 f5 f6 f7 a) m r)
             -> StateT (Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n  a) m r
mapStatePoly prf f = do
  e <- get
  (a, s) <- lift $ unPairT $ mapPolyF prf (\r -> PairT $ runStateT f r) e
  put s
  return a

foldPoly :: forall f0 f1 f2 f3 f4 f5 f6 f7
                   a k r n
         .  ( k f0, k f1, k f2, k f3
            , k f4, k f5, k f6, k f7)
         => (forall g . k g :- Functor8 g)
         -> (forall f. k f
             => Cofree8Inner' f  f0 f1 f2 f3 f4 f5 f6 f7 a
             -> r)
         -> Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n  a
         -> r
foldPoly prf f = \case
      CF0 a r -> f r \\ (prf :: k f0 :- Functor8 f0)
      CF1 a r -> f r \\ (prf :: k f1 :- Functor8 f1)
      CF2 a r -> f r \\ (prf :: k f2 :- Functor8 f2)
      CF3 a r -> f r \\ (prf :: k f3 :- Functor8 f3)
      CF4 a r -> f r \\ (prf :: k f4 :- Functor8 f4)
      CF5 a r -> f r \\ (prf :: k f5 :- Functor8 f5)
      CF6 a r -> f r \\ (prf :: k f6 :- Functor8 f6)
      CF7 a r -> f r \\ (prf :: k f7 :- Functor8 f7)


mapAll :: ( Functor8 f0, Functor8 f1, Functor8 f2, Functor8 f3
          , Functor8 f4, Functor8 f5, Functor8 f6, Functor8 f7)
       => (a -> b)
       -> Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n  a
       -> Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n  b
mapAll f = map8 f f f f f f f f

foldMapAll :: ( Foldable8 f0, Foldable8 f1, Foldable8 f2, Foldable8 f3
              , Foldable8 f4, Foldable8 f5, Foldable8 f6, Foldable8 f7)
           => Monoid m
           => (a -> m)
           -> Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n  a
           -> m
foldMapAll f = foldMap8 f f f f f f f f

traverseAll :: ( Traversable8 f0, Traversable8 f1, Traversable8 f2, Traversable8 f3
               , Traversable8 f4, Traversable8 f5, Traversable8 f6, Traversable8 f7)
            => Applicative f
            => (a -> f b)
            -> Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n  a
            -> f (Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n  b)
traverseAll f = traverse8 f f f f f f f f


instance ( Functor8 f0, Functor8 f1, Functor8 f2, Functor8 f3
         , Functor8 f4, Functor8 f5, Functor8 f6, Functor8 f7)
         => Functor (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  7  a0 a1 a2 a3 a4 a5 a6) where
  fmap f = map8 id id id id id id id f


instance forall f0 f1 f2 f3 f4 f5 f6 f7 a0 a1 a2 a3 a4 a5 a6
         . ( Functor8 f0, Functor8 f1, Functor8 f2, Functor8 f3
           , Functor8 f4, Functor8 f5, Functor8 f6, Functor8 f7)
         => Comonad (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  7  a0 a1 a2 a3 a4 a5 a6) where
  extract (CF7 a _) = a

  duplicate = dupAll

dupAll :: forall f0 f1 f2 f3 f4 f5 f6 f7  n  a0 a1 a2 a3 a4 a5 a6 a7
       .  ( Functor8 f0, Functor8 f1, Functor8 f2, Functor8 f3
          , Functor8 f4, Functor8 f5, Functor8 f6, Functor8 f7)
       => (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  n  a0 a1 a2 a3 a4 a5 a6 a7)
       -> (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  n  a0 a1 a2 a3 a4 a5 a6
            (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  7  a0 a1 a2 a3 a4 a5 a6 a7))
dupAll = \case
  CF0 a r     -> CF0 a $ mapFPoly dupAll r
  CF1 a r     -> CF1 a $ mapFPoly dupAll r
  CF2 a r     -> CF2 a $ mapFPoly dupAll r
  CF3 a r     -> CF3 a $ mapFPoly dupAll r
  CF4 a r     -> CF4 a $ mapFPoly dupAll r
  CF5 a r     -> CF5 a $ mapFPoly dupAll r
  CF6 a r     -> CF6 a $ mapFPoly dupAll r
  w@(CF7 _ r) -> CF7 w $ mapFPoly dupAll r

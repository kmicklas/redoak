{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
-- TODO Generalize 8 to N
module Control.Comonad.Cofree8 where

import GHC.TypeLits

import Data.Functor8

data Cofree8
       f0 f1 f2 f3 f4 f5 f6 f7
       (n :: Nat)
       a0 a1 a2 a3 a4 a5 a6 a7
       :: * where

  CF0 :: a0
      -> (f0 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  0  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  1  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  2  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  3  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  4  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  5  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  6  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  7  a0 a1 a2 a3 a4 a5 a6 a7))
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  0  a0 a1 a2 a3 a4 a5 a6 a7

  CF1 :: a1
      -> (f1 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  0  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  1  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  2  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  3  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  4  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  5  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  6  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  7  a0 a1 a2 a3 a4 a5 a6 a7))
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  1  a0 a1 a2 a3 a4 a5 a6 a7

  CF2 :: a2
      -> (f2 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  0 a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  1  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  2  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  3  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  4  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  5  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  6  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  7  a0 a1 a2 a3 a4 a5 a6 a7))
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  2  a0 a1 a2 a3 a4 a5 a6 a7

  CF3 :: a3
      -> (f3 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  0  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  1  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  2  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  3  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  4  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  5  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  6  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  7  a0 a1 a2 a3 a4 a5 a6 a7))
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  3  a0 a1 a2 a3 a4 a5 a6 a7

  CF4 :: a4
      -> (f4 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  0  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  1  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  2  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  3  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  4  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  5  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  6  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  7  a0 a1 a2 a3 a4 a5 a6 a7))
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  4  a0 a1 a2 a3 a4 a5 a6 a7

  CF5 :: a5
      -> (f5 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  0  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  1  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  2  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  3  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  4  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  5  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  6  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  7  a0 a1 a2 a3 a4 a5 a6 a7))
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  5  a0 a1 a2 a3 a4 a5 a6 a7

  CF6 :: a6
      -> (f6 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  0  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  1  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  2  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  3  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  4  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  5  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  6  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  7  a0 a1 a2 a3 a4 a5 a6 a7))
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  6  a0 a1 a2 a3 a4 a5 a6 a7

  CF7 :: a7
      -> (f7 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  0  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  1  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  2  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  3  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  4  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  5  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  6  a0 a1 a2 a3 a4 a5 a6 a7)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  7  a0 a1 a2 a3 a4 a5 a6 a7))
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  7  a0 a1 a2 a3 a4 a5 a6 a7


instance forall f0 f1 f2 f3 f4 f5 f6 f7 n
         . ( Functor8 f0
           , Functor8 f1
           , Functor8 f2
           , Functor8 f3
           , Functor8 f4
           , Functor8 f5
           , Functor8 f6
           , Functor8 f7)
         => Functor8 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7 n) where
  map8 :: forall a0 a1 a2 a3 a4 a5 a6 a7
                 b0 b1 b2 b3 b4 b5 b6 b7
                 n
       .  (a0 -> b0)
       -> (a1 -> b1)
       -> (a2 -> b2)
       -> (a3 -> b3)
       -> (a4 -> b4)
       -> (a5 -> b5)
       -> (a6 -> b6)
       -> (a7 -> b7)
       -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  n  a0 a1 a2 a3 a4 a5 a6 a7
       -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  n  b0 b1 b2 b3 b4 b5 b6 b7
  map8 f0 f1 f2 f3 f4 f5 f6 f7 = go where
    go :: forall m
       .  Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  m  a0 a1 a2 a3 a4 a5 a6 a7
       -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  m  b0 b1 b2 b3 b4 b5 b6 b7
    go = \case
      CF0 a r -> CF0 (f0 a) (map8 go go go go go go go go r)
      CF1 a r -> CF1 (f1 a) (map8 go go go go go go go go r)
      CF2 a r -> CF2 (f2 a) (map8 go go go go go go go go r)
      CF3 a r -> CF3 (f3 a) (map8 go go go go go go go go r)
      CF4 a r -> CF4 (f4 a) (map8 go go go go go go go go r)
      CF5 a r -> CF5 (f5 a) (map8 go go go go go go go go r)
      CF6 a r -> CF6 (f6 a) (map8 go go go go go go go go r)
      CF7 a r -> CF7 (f7 a) (map8 go go go go go go go go r)

type Cofree8' f0 f1 f2 f3 f4 f5 f6 f7   n  a =
  Cofree8 f0 f1 f2 f3 f4 f5 f6 f7
          n
          a  a  a  a  a  a  a  a
          

mapAll :: _
       => (a -> b)
       -> Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n  a
       -> Cofree8' f0 f1 f2 f3 f4 f5 f6 f7  n  b
mapAll f = map8 f f f f f f f f

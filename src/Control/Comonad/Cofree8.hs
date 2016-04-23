{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
-- TODO Generalize 8 to N
module Control.Comonad.Cofree8 where

import GHC.TypeLits

data Cofree8
       f0 f1 f2 f3 f4 f5 f6 f7
       a0 a1 a2 a3 a4 a5 a6 a7
       (n :: Nat)
       :: * where

  CF0 :: a0
      -> (f0 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  0)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  1)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  2)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  3)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  4)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  5)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  6)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  7))
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7 0

  CF1 :: a1
      -> (f1 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  0)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  1)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  2)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  3)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  4)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  5)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  6)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  7))
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7 1

  CF2 :: a2
      -> (f2 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  0)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  1)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  2)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  3)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  4)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  5)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  6)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  7))
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7 2

  CF3 :: a3
      -> (f3 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  0)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  1)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  2)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  3)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  4)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  5)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  6)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  7))
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7 3

  CF4 :: a4
      -> (f4 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  0)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  1)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  2)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  3)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  4)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  5)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  6)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  7))
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7 4

  CF5 :: a5
      -> (f5 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  0)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  1)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  2)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  3)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  4)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  5)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  6)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  7))
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7 5

  CF6 :: a6
      -> (f6 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  0)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  1)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  2)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  3)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  4)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  5)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  6)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  7))
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7 6

  CF7 :: a7
      -> (f7 (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  0)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  1)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  2)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  3)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  4)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  5)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  6)
             (Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7  7))
      -> Cofree8 f0 f1 f2 f3 f4 f5 f6 f7  a0 a1 a2 a3 a4 a5 a6 a7 7

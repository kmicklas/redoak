{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
-- TODO Generalize 5 to N
module Control.Comonad.Cofree5 where

import GHC.TypeLits

data Cofree5
       f0 f1 f2 f3 f4
       a0 a1 a2 a3 a4
       (n :: Nat)
       :: * where

  CF0 :: a0
      -> (f0 (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 0)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 1)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 2)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 3)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 4))
      -> Cofree5 f0 f1 f2 f3 f4 a0 a1 a2 a3 a4 0

  CF1 :: a1
      -> (f1 (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 0)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 1)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 2)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 3)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 4))
      -> Cofree5 f0 f1 f2 f3 f4 a0 a1 a2 a3 a4 1

  CF2 :: a2
      -> (f2 (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 0)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 1)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 2)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 3)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 4))
      -> Cofree5 f0 f1 f2 f3 f4 a0 a1 a2 a3 a4 2

  CF3 :: a3
      -> (f3 (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 0)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 1)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 2)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 3)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 4))
      -> Cofree5 f0 f1 f2 f3 f4 a0 a1 a2 a3 a4 3

  CF4 :: a4
      -> (f4 (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 0)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 1)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 2)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 3)
             (Cofree5 f0 f1 f2 f3 f4  a0 a1 a2 a3 a4 4))
      -> Cofree5 f0 f1 f2 f3 f4 a0 a1 a2 a3 a4 4

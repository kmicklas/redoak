{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
module Data.Functor8 where

import Data.Coerce

class Functor8 f where
  map8 :: forall
          a0 a1 a2 a3 a4 a5 a6 a7
          b0 b1 b2 b3 b4 b5 b6 b7
       .  (a0 -> b0)
       -> (a1 -> b1)
       -> (a2 -> b2)
       -> (a3 -> b3)
       -> (a4 -> b4)
       -> (a5 -> b5)
       -> (a6 -> b6)
       -> (a7 -> b7)
       -> (f a0 a1 a2 a3 a4 a5 a6 a7 -> f b0 b1 b2 b3 b4 b5 b6 b7)


data Void8 a0 a1 a2 a3 a4 a5 a6 a7
deriving instance Eq (Void8 a0 a1 a2 a3 a4 a5 a6 a7)
deriving instance Ord (Void8 a0 a1 a2 a3 a4 a5 a6 a7)
deriving instance Show (Void8 a0 a1 a2 a3 a4 a5 a6 a7)

instance Functor8 Void8 where
  map8 _ _ _ _ _ _ _ _ x = case x of { }


data Const8 x a0 a1 a2 a3 a4 a5 a6 a7 = Const8 x

instance Functor8 (Const8 x) where
  map8 _ _ _ _ _ _ _ _ = coerce

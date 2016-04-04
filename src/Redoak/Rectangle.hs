{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Redoak.Rectangle
  ( Width(..)
  , Height(..)
  , X(..)
  , Y(..)
  , Dimensions
  , Position
  , origin
  ) where

newtype Width n = W n
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

newtype Height n = H n
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

newtype X n = X n
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

newtype Y n = Y n
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

type Dimensions n = (Width n, Height n)
type Position   n = (X n, Y n)

origin :: Num n => (X n, Y n)
origin = (X 0, Y 0)

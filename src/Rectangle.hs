{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rectangle
  ( Width(..)
  , Height(..)
  , X(..)
  , Y(..)
  , Dimensions
  , Position
  , origin
  ) where

newtype Width = Width Int
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

newtype Height = Height Int
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

newtype X = X Int
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

newtype Y = Y Int
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

type Dimensions = (Width, Height)
type Position   = (X, Y)

origin = (X 0, Y 0)

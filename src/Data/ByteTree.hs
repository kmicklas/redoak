module Data.ByteTree where

import Control.Monad.Free
import Data.ByteString
import Data.Sequence

type ByteNode = Free Seq ByteString
newtype ByteTree = ByteTree (Seq ByteNode)

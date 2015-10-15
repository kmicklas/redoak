module Tree
  ( Tree
  , Element(..)
  ) where

import Data.Sequence as S

type Tree = Seq Element

data Element
  = Char !Char
  | Node !Tree
  deriving (Eq, Ord)

type Path = ([Int], Int, Int)

type Edit = (Tree, Path) -> (Tree, Path)

switchBounds :: Edit
switchBounds (t, (p, start, end)) = (t, (p, end, start))

selectAll :: Edit
selectAll (t, ([], _, _)) = (t, ([], 0, S.length t))
selectAll (t, (c : cs, start, end) = selectAll

selectNone :: Edit

shiftLeft :: Edit
shiftLeft ([], 

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module Tree
  ( Tree
  , Element(..)
  ) where

import Data.Foldable
import Data.Sequence as S
import Data.Text     as T hiding (foldr)
import Data.Word

type Tree i a = Seq (Element i a)

data Element i a
  = Atom { ident :: i, value :: a }
  | Node { ident :: i, children :: (Tree i a) }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Range = (Int, Int)

data Path
  = Path
    { indices :: [Int]
    , bounds :: Range
    }

type Edit i a = (Tree i a, Path) -> (Tree i a, Path)

stringify :: Tree i Char -> Tree i String
stringify = fromList . foldr join []
  where join (Atom i c) (Atom _ cs : rest) = Atom i (c : cs) : rest
        join (Atom i c) rest = Atom i [c] : rest
        join (Node i ts) rest = Node i (stringify ts) : rest

change :: Tree i a -> Edit i a
change new (t, Path [] (start, end)) = (t', Path [] (start', end'))
  where t' = S.take (min start end) t >< new >< S.drop (max start end) t
        (start', end') =
          if start <= end
          then (start, start + S.length new)
          else (end + S.length new, end)
change new (t, Path (i : is) b) = (t', Path (i : is) b')
  where t' = S.update i (Node (ident $ S.index t i) sub) t
        (sub, Path _ b') = change new (children $ S.index t i, Path is b)

localMove :: (Int -> Range -> Range) -> Edit i a
localMove f (t, Path is b) = (t, Path is $ fix t $ move t is b)
  where move t [] b = f (S.length t) b
        move t (i : is) b = move (children $ S.index t i) is b
        fix t (start, end) =
          if min start end < 0 || max start end > S.length t
          then b
          else (start, end)

switchBounds :: Edit i a
switchBounds = localMove $ \ _ (start, end) -> (end, start)

startMin :: Edit i a
startMin = localMove $ \ _ (_, end) -> (0, end)

endMax :: Edit i a
endMax = localMove $ \ size (start, _) -> (start, size)

selectNoneStart :: Edit i a
selectNoneStart = localMove $ \ _ (start, _) -> (start, start)

selectNoneEnd :: Edit i a
selectNoneEnd = localMove $ \ _ (_, end) -> (end, end)

shiftLeft :: Edit i a
shiftLeft = localMove $ \ _ (start, end) -> (start + 1, end + 1)

shiftRight :: Edit i a
shiftRight = localMove $ \ _ (start, end) -> (start - 1, end - 1)

moveLeft :: Edit i a
moveLeft = localMove $ \ _ (start, end) -> (start, end + 1)

moveRight :: Edit i a
moveRight = localMove $ \ _ (start, end) -> (start, end - 1)

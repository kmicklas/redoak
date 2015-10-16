{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module Tree
  ( Tree
  , Element(..)
  ) where

import Data.Foldable
import Data.Sequence as S
import Data.Text     as T
import Data.Word

type Tree a = Seq (Element a)

data Element a
  = Atom a
  | Node { children :: (Tree a) }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Range = (Int, Int)

data Path
  = Path
    { indices :: [Int]
    , bounds :: Range
    }

type Edit a = (Tree a, Path) -> (Tree a, Path)

stringify :: Tree Char -> Tree Text
stringify = fmap stringifyElem

stringifyElem :: Element Char -> Element Text
stringifyElem = \case
  Atom char   -> Atom $ T.singleton char
  Node forest -> case mapM atom forest of
    Just seqString -> Atom $ T.pack $ toList seqString
    Nothing        -> Node $ stringify forest
  where atom :: Element Char -> Maybe Char
        atom = \case
          Atom c -> Just c
          Node _ -> Nothing

change :: Tree a -> Edit a
change new (t, Path [] (start, end)) = (t', Path [] (start', end'))
  where t' = S.take (min start end) t >< new >< S.drop (max start end) t
        (start', end') =
          if start <= end
          then (start, start + S.length new)
          else (end + S.length new, end)
change new (t, Path (i : is) b) = (t', Path (i : is) b')
  where t' = S.update i (Node sub) t
        (sub, Path _ b') = change new (children $ S.index t i, Path is b)

localMove :: (Int -> Range -> Range) -> Edit a
localMove f (t, Path is b) = (t, Path is $ fix t $ move t is b)
  where move t [] b = f (S.length t) b
        move t (i : is) b = move (children $ S.index t i) is b
        fix t (start, end) =
          if min start end < 0 || max start end > S.length t
          then b
          else (start, end)

switchBounds :: Edit a
switchBounds = localMove $ \ _ (start, end) -> (end, start)

startMin :: Edit a
startMin = localMove $ \ _ (_, end) -> (0, end)

endMax :: Edit a
endMax = localMove $ \ size (start, _) -> (start, size)

selectNoneStart :: Edit a
selectNoneStart = localMove $ \ _ (start, _) -> (start, start)

selectNoneEnd :: Edit a
selectNoneEnd = localMove $ \ _ (_, end) -> (end, end)

shiftLeft :: Edit a
shiftLeft = localMove $ \ _ (start, end) -> (start + 1, end + 1)

shiftRight :: Edit a
shiftRight = localMove $ \ _ (start, end) -> (start - 1, end - 1)

moveLeft :: Edit a
moveLeft = localMove $ \ _ (start, end) -> (start, end + 1)

moveRight :: Edit a
moveRight = localMove $ \ _ (start, end) -> (start, end - 1)

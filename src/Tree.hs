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
  | Node (Tree a)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

type Path = ([Word], Word, Word)

type Edit = (Tree Char, Path) -> (Tree Char, Path)

{-
switchBounds :: Edit
switchBounds (t, (p, start, end)) = (t, (p, end, start))

selectAll :: Edit
selectAll (t, ([], _, _)) = (t, ([], 0, S.length t))
selectAll (t, (c : cs, start, end)) = selectAll

selectNone :: Edit

shiftLeft :: Edit
shiftLeft ([],
-}

stringify ::  Tree Char -> Tree Text
stringify = fmap stringifyElem

stringifyElem :: Element Char -> Element Text
stringifyElem = \case
  Atom char -> Atom $ T.singleton char
  Node forest -> case mapM atom forest of
    Just seqString -> Atom $ T.pack $ toList seqString
    Nothing        -> Node $ stringifyElem <$> forest
  where atom :: Element Char -> Maybe Char
        atom = \case
          Atom c -> Just c
          Node _ -> Nothing

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module Tree
  ( Tree
  , Element(..)
  , Range
  , Path(..)
  , Cursor(..)
  , Edit
  , compress
  , stringify
  , change
  , switchBounds
  , startMin
  , endMax
  , selectNoneStart
  , selectNoneEnd
  , shiftLeft
  , shiftRight
  , moveLeft
  , moveRight
  ) where

import Data.Foldable
import Data.Traversable
import Data.Maybe
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
  deriving (Eq, Ord, Show)

data Cursor i a
  = Cursor
    { tree :: Tree i a
    , selection :: Path
    }
  deriving (Eq, Ord, Show)

type Edit i a = Cursor i a -> Cursor i a

compress :: Monoid a => Tree i a -> Tree i a
compress seq = fromMaybe seq $ do
  Atom i _ :< _ <- return $ viewl seq -- ensure at least 1 atom; fail _ = Nothing
  allAtoms <- for seq $ \case
    Atom _ a -> Just a
    _        -> Nothing
  return $ S.singleton $ Atom i $ fold allAtoms

stringify :: Tree i Char -> Tree i Text
stringify = compress . (fmap . fmap) T.singleton

charJoin :: Char -> Maybe String -> String
charJoin c Nothing = [c]
charJoin c (Just cs) = c : cs

change :: Tree i a -> Edit i a
change new (Cursor t (Path [] (start, end))) =
  Cursor t' $ Path [] (start', end')
  where t' = S.take (min start end) t >< new >< S.drop (max start end) t
        (start', end') =
          if start <= end
          then (start, start + S.length new)
          else (end + S.length new, end)
change new (Cursor t (Path (i : is) b)) = Cursor t' $ Path (i : is) b'
  where t' = S.update i (Node (ident $ S.index t i) sub) t
        Cursor sub (Path _ b') = change new $ Cursor (children $ S.index t i) $ Path is b

localMove :: (Int -> Range -> Range) -> Edit i a
localMove f (Cursor t (Path is b)) = Cursor t $ Path is $ fix t $ move t is b
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
shiftLeft = localMove $ \ _ (start, end) -> (start - 1, end - 1)

shiftRight :: Edit i a
shiftRight = localMove $ \ _ (start, end) -> (start + 1, end + 1)

moveLeft :: Edit i a
moveLeft = localMove $ \ _ (start, end) -> (start, end + 1)

moveRight :: Edit i a
moveRight = localMove $ \ _ (start, end) -> (start, end - 1)

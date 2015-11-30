{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE ViewPatterns #-}

module Tree
  ( Tree
  , Element(..)
  , Range
  , Path(..)
  , Cursor(..)

  , Edit
  , EditT
  , EditM
  , freshId

  , compress
  , stringify
  , change

  , pop
  , push

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

import Control.Monad.Trans.State.Lazy

import Data.Foldable
import Data.Functor.Identity
import Data.Monoid
import Data.Traversable
import Data.List     as L
import Data.Maybe    as M
import Data.Sequence as S
import Data.Text     as T hiding (foldr)
import Data.Word

type Tree a v = Seq (Element a v)

data Element a v
  = Atom { ann :: a, value :: v }
  | Node { ann :: a, children :: (Tree a v) }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Range = (Int, Int)

data Path
  = Path
    { indices :: [Int]
    , bounds :: Range
    }
  deriving (Eq, Ord, Show)

data Cursor a v
  = Cursor
    { tree :: Tree a v
    , selection :: Path
    }
  deriving (Eq, Ord, Show)

type Edit a v = Cursor a v -> Cursor a v
type EditT m a v = Cursor a v -> StateT a m (Cursor a v)
type EditM a v = EditT Identity a v

freshId :: Num i => State i i
freshId = do
  i <- get
  put $ i + 1
  return i

newtype JoinTree a v = Join { getJoined :: (Tree a v) }

instance Monoid v => Monoid (JoinTree a v) where
  mempty = Join S.empty
  mappend (Join (viewr -> l :> Atom a v)) (Join (viewl -> Atom _ w :< r)) =
    Join $ l >< (S.singleton $ Atom a (v <> w)) >< r
  mappend (Join l) (Join r) = Join $ l >< r

compress :: Monoid v => Tree a v -> Tree a v
compress = getJoined . foldMap (Join . S.singleton . compressInner)
  where compressInner = \case
          Atom a v -> Atom a v
          Node i t -> Node i $ compress t

stringify :: Tree i Char -> Tree i Text
stringify = compress . (fmap . fmap) T.singleton

charJoin :: Char -> Maybe String -> String
charJoin c Nothing = [c]
charJoin c (Just cs) = c : cs

change :: Tree a v -> EditM a v
change new = \case

  Cursor t (Path [] (start, end)) ->
    return $ Cursor t' $ Path [] (start', end')
    where t' = S.take (min start end) t >< new >< S.drop (max start end) t
          (start', end') =
            if start <= end
            then (start, start + S.length new)
            else (end + S.length new, end)

  Cursor t (Path (i : is) b) -> do
    Cursor sub (Path _ b') <- change new $
      Cursor (children $ S.index t i) $ Path is b
    let t' = S.update i (Node (ann $ S.index t i) sub) t
    return $ Cursor t' $ Path (i : is) b'

localMove :: (Int -> Range -> Range) -> EditM a v
localMove f (Cursor t (Path is b)) = return $ Cursor t $ Path is $ move t is b
  where move t [] b = fix t $ f (S.length t) b
        move t (i : is) b = move (children $ S.index t i) is b
        fix t (start, end) =
          if min start end < 0 || max start end > S.length t
          then b
          else (start, end)

-- | Go back to editing parent, right of current position
-- | new parent if at root
pop :: Num i => EditM a v
pop = \case
  c@(Cursor t (Path [] bounds)) -> return c
  Cursor t (Path is (_, _)) -> return $
    Cursor t $ Path (L.init is) (L.last is + 1, L.last is + 1)

-- | Create new node, edit at begining of it
push :: Num a => EditM a v
push c = do
  i <- freshId
  Cursor t (Path stack (start, end)) <- change (S.singleton $ Node i S.empty) c
  return $ Cursor t $ Path (stack ++ [min start end]) (0, 0)


switchBounds :: EditM a v
switchBounds = localMove $ \ _ (start, end) -> (end, start)

startMin :: EditM a v
startMin = localMove $ \ _ (_, end) -> (0, end)

endMax :: EditM a v
endMax = localMove $ \ size (start, _) -> (start, size)

selectNoneStart :: EditM a v
selectNoneStart = localMove $ \ _ (start, _) -> (start, start)

selectNoneEnd :: EditM a v
selectNoneEnd = localMove $ \ _ (_, end) -> (end, end)

shiftLeft :: EditM a v
shiftLeft = localMove $ \ _ (start, end) -> (start - 1, end - 1)

shiftRight :: EditM a v
shiftRight = localMove $ \ _ (start, end) -> (start + 1, end + 1)

moveLeft :: EditM a v
moveLeft = localMove $ \ _ (start, end) -> (start, end + 1)

moveRight :: EditM a v
moveRight = localMove $ \ _ (start, end) -> (start, end - 1)

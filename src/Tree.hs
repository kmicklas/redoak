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
type EditT m i a = Cursor i a -> StateT i m (Cursor i a)
type EditM i a = EditT Identity i a

newtype JoinTree i a = Join { getJoined :: (Tree i a) }

instance Monoid a => Monoid (JoinTree i a) where
  mempty = Join S.empty
  mappend (Join (viewr -> l :> Atom i a)) (Join (viewl -> Atom _ b :< r)) =
    Join $ l >< (S.singleton $ Atom i (a <> b)) >< r
  mappend (Join l) (Join r) = Join $ l >< r

compress :: Monoid a => Tree i a -> Tree i a
compress = getJoined . foldMap (Join . S.singleton . compressInner)
  where compressInner = \case
          Atom i a -> Atom i a
          Node i t -> Node i $ compress t

stringify :: Tree i Char -> Tree i Text
stringify = compress . (fmap . fmap) T.singleton

charJoin :: Char -> Maybe String -> String
charJoin c Nothing = [c]
charJoin c (Just cs) = c : cs

change :: Tree i a -> Edit i a
change new = \case

  (Cursor t (Path [] (start, end))) -> Cursor t' $ Path [] (start', end')
    where t' = S.take (min start end) t >< new >< S.drop (max start end) t
          (start', end') =
            if start <= end
            then (start, start + S.length new)
            else (end + S.length new, end)

  (Cursor t (Path (i : is) b)) -> Cursor t' $ Path (i : is) b'
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


freshID :: Num i => State i i
freshID = do
  i <- get
  put $ i + 1
  return i

-- | Go back to editing parent, right of current position
-- | new parent if at root
pop :: Num i => EditM i a
pop (Cursor t (Path stack (_, _))) = do
  i <- freshID
  return $ case stack of
    []    -> Cursor (S.singleton $ Node i t) $ Path [] (1, 1)
    stack -> Cursor t                        $ Path (L.reverse r) (f, f)
      where (f:r) = L.reverse stack

-- | Create new node, edit at begining of it
-- FIX bounds, I don't get why we don't impose <= invariant
push :: Num i => EditM i a
push c = do
  i <- freshID
  let (Cursor t (Path stack (x, _))) = change (S.singleton $ Node i S.empty) c
  return $ Cursor t $ Path (stack ++ [x]) (0, 0)


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

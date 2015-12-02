{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE OverloadedLists #-}

module Tree
  ( Tree
  , Ann(..)
  , Element(..)
  , Range
  , Path(..)
  , Cursor(..)

  , Edit
  , EditT
  , EditM
  , freshId

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
import Data.Text     as T
import Data.Word

data Ann a ann
  = (:=) { ann :: ann , val :: a }
  deriving (Eq, Ord, Show)

data Element a ann
  = Atom { value :: a }
  | Node { children :: Seq (Tree a ann) }
  deriving (Eq, Ord, Show)

type Tree a ann = Ann (Element a ann) ann

type Range = (Int, Int)

data Path
  = Path
    { indices :: [Int]
    , bounds :: Range
    }
  deriving (Eq, Ord, Show)

data Cursor a ann
  = Cursor
    { tree :: Tree a ann
    , selection :: Path
    }
  deriving (Eq, Ord, Show)

type Edit a ann = Cursor a ann -> Cursor a ann
type EditT m a ann = Cursor a ann -> StateT ann m (Cursor a ann)
type EditM a ann = EditT Identity a ann

freshId :: Num i => State i i
freshId = do
  i <- get
  put $ i + 1
  return i

change :: Num ann => Element Text ann -> EditM Text ann
change = flip $ \case

  Cursor (id := Atom v) (Path [] (start, end)) -> \case
    Atom new ->
      return $ Cursor (id := Atom v') $ Path [] (start', end')
      where v' = mconcat [lPart, new, rPart]
            (start', end') =
              if start <= end
              then (start, start + T.length new)
              else (end + T.length new, end)

    Node new -> do
      lId <- freshId
      rId <- freshId
      let cs' = [lId := Atom lPart] >< new >< [rId := Atom rPart]
      return $ Cursor (id := Node cs') $ Path [] (start', end')
      where (start', end') =
              if start <= end
              then (start, start + S.length new)
              else (end + S.length new, end)

    where lPart = T.take (min start end) v 
          rPart = T.drop (max start end) v 

  c@(Cursor (id := Node cs) (Path [] (start, end))) -> \case
    Atom new -> do
      id <- freshId
      change (Node [id := Atom new]) c

    Node new ->
      return $ Cursor (id := Node cs') $ Path [] (start', end')
      where cs' = S.take (min start end) cs >< new >< S.drop (max start end) cs
            (start', end') =
              if start <= end
              then (start, start + S.length new)
              else (end + S.length new, end)

  Cursor (id := Node cs) (Path (i : is) b) -> \ new -> do
    Cursor sub (Path _ b') <- change new $
      Cursor (S.index cs i) $ Path is b
    let cs' = S.update i sub cs
    return $ Cursor (id := Node cs') $ Path (i : is) b'

localMove :: (Int -> Range -> Range) -> EditM Text ann
localMove f (Cursor t (Path is b)) =
  return $ Cursor t $ Path is $ move (val t) is b
  where move e [] b = fix e $ f (S.length $ children e) b
        move e (i : is) b = move (val $ S.index (children e) i) is b
        fix (Atom v) (start, end) =
          if min start end < 0 || max start end > T.length v
          then b
          else (start, end)
        fix (Node cs) (start, end) =
          if min start end < 0 || max start end > S.length cs
          then b
          else (start, end)

-- | Go back to editing parent, right of current position
-- | new parent if at root
pop :: EditM a ann
pop = \case
  c@(Cursor t (Path [] bounds)) -> return c
  Cursor t (Path is (_, _)) -> return $
    Cursor t $ Path (L.init is) (L.last is + 1, L.last is + 1)

-- | Create new node, edit at begining of it
push :: Num ann => EditM Text ann
push c = do
  id <- freshId
  Cursor t (Path stack (start, end)) <- change (Node [id := Node []]) c
  return $ Cursor t $ Path (stack ++ [min start end]) (0, 0)


switchBounds :: EditM Text ann
switchBounds = localMove $ \ _ (start, end) -> (end, start)

startMin :: EditM Text ann
startMin = localMove $ \ _ (_, end) -> (0, end)

endMax :: EditM Text ann
endMax = localMove $ \ size (start, _) -> (start, size)

selectNoneStart :: EditM Text ann
selectNoneStart = localMove $ \ _ (start, _) -> (start, start)

selectNoneEnd :: EditM Text ann
selectNoneEnd = localMove $ \ _ (_, end) -> (end, end)

shiftLeft :: EditM Text ann
shiftLeft = localMove $ \ _ (start, end) -> (start - 1, end - 1)

shiftRight :: EditM Text ann
shiftRight = localMove $ \ _ (start, end) -> (start + 1, end + 1)

moveLeft :: EditM Text ann
moveLeft = localMove $ \ _ (start, end) -> (start, end + 1)

moveRight :: EditM Text ann
moveRight = localMove $ \ _ (start, end) -> (start, end - 1)

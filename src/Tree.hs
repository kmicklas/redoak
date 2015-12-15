{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}

module Tree
  ( Tree(..)
  , Trunk
  , Ann(..)
  , Element(..)
  , Range
  , Selection(..)
  , Path
  , Cursor

  , Edit
  , EditT
  , EditM
  , mapEdit
  , freshId
  , path

  , change
  , insertNode
  , descend
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

import Control.Monad
import Control.Monad.Trans.State.Lazy

import Data.Bifunctor
import Data.Bifunctor.TH
import Data.Foldable
import Data.Functor.Identity
import Data.Monoid
import Data.Traversable
import Data.MonoTraversable hiding (Element)
import Data.List      as L
import Data.Maybe     as M
import Data.Sequence  as S
import Data.Sequences as SS
import Data.Text      as T
import Data.Word

data Ann a ann
  = (:=) { ann :: ann , val :: a }
  deriving (Eq, Ord, Show)
deriveBifunctor ''Ann
deriveBifoldable ''Ann
deriveBitraversable ''Ann

data Element a b
  = Atom { value :: a }
  | Node { children :: Seq b }
  deriving (Eq, Ord, Show)
deriveBifunctor ''Element
deriveBifoldable ''Element
deriveBitraversable ''Element

type Trunk a ann = Element a (Tree a ann)

newtype Tree a ann
  = T { unTree :: Ann (Trunk a ann) ann }
  deriving (Eq, Ord, Show)
deriveBifunctor ''Tree
deriveBifoldable ''Tree
deriveBitraversable ''Tree

type Range = (Int, Int)

data Selection
  = Descend Int
  | Select Range
  deriving (Eq, Ord, Show)

type Path = ([Int], Range)

type Cursor a ann = Tree a (ann, Selection)

type Edit a ann = Cursor a ann -> Cursor a ann
type EditT m a ann = Cursor a ann -> StateT ann m (Cursor a ann)
type EditM a ann = EditT Identity a ann

mapEdit :: (m (Cursor a ann, ann) -> n (Cursor a ann, ann))
         -> EditT m a ann -> EditT n a ann
mapEdit = fmap . mapStateT

freshId :: Num i => State i i
freshId = do
  i <- get
  put $ i + 1
  return i

path :: Cursor a ann -> Path
path (T ((_, Descend i) := Node cs)) = first (i :) $ path $ S.index cs i
path (T ((_, Select r) := _)) = ([], r)

elimIsSequence :: forall seq x ret
               .  IsSequence seq
               => (forall s. IsSequence s => s -> ret)
               -> Element seq x
               -> ret
elimIsSequence f = \case
  Atom a -> f a
  Node s -> f s

localEdit :: Monad m => EditT m a ann -> EditT m a ann
localEdit f t@(T ((a, sel) := e)) =
  case (sel, e) of
    (Descend _, Atom _)  -> error "path is too deep"
    (Descend i, Node ts) -> do
      child <- localEdit f $ S.index ts i
      return $ T $ (a, sel) := Node (S.update i child ts)
    (Select range, _) -> f t

localMove :: (Int -> Range -> Range) -> EditM Text ann
localMove f = localEdit $ \ (T ((a, Select r) := e)) ->
  return $ T $ (a, Select $ f (elimIsSequence olength e) r) := e

change :: forall ann atom
       .  Num ann
       => IsSequence atom
       => Trunk atom (ann, Selection)
       -> EditM atom ann
change new = localEdit $ \ (T ((a, Select (start, end)) := old)) -> let
    insert :: forall seq. IsSequence seq
      => seq
      -> seq
      -> (seq -> Trunk atom (ann, Selection))
      -> StateT ann Identity (Cursor atom ann)
    insert old new inj = return $ T $ (a, Select $ adjustRange new) := inj seq'
      where seq' = mconcat [lPart, new, rPart]
            (lPart, rPart) = split old

    split :: forall seq. IsSequence seq => seq -> (seq, seq)
    split old = ( SS.take (fromIntegral $ min start end) old
                , SS.drop (fromIntegral $ max start end) old
                )

    adjustRange :: forall seq. IsSequence seq => seq -> Range
    adjustRange new = if start <= end
            then (start, start + olength new)
            else (end + olength new, end)

  in case (old, new) of
    -- homogenous
    (Atom o, Atom n) -> insert o n Atom
    (Node o, Node n) -> insert o n Node
    -- heterogenous
    (Node o, Atom _) -> do
      id <- freshId
      insert o [T $ (id, Select (0, 0)) := new] Node
    (Atom o, Node n) -> do
      lId <- freshId
      rId <- freshId
      let cs = [T $ init lId := Atom lPart] >< n >< [T $ init rId := Atom rPart]
      return $ T $ (a, Select (start', end')) := Node cs
        where (lPart, rPart) = split o
              (start', end') = adjustRange n

    where init ann = (ann, Select (0, 0))

-- | Go back to editing parent, right of current position
-- | new parent if at root
pop :: EditT Maybe a ann
pop (T ((a, Select _) := _)) = mzero
pop (T ((a, Descend i) := Node cs)) =
  case S.index cs i of
    T ((a, Select _) := _) -> return $ T $ (a, Select (i + 1, i + 1)) := Node cs
    t@(T ((a, Descend _) := _)) -> do
      sub <- pop t
      return $ T $ (a, Descend i) := Node (update i sub cs)

-- | Insert a new empty node at the cursor
insertNode :: (IsSequence a, Num ann) => EditM a ann
insertNode c = do
  id <- freshId
  change (Node [T $ (id, Select (0, 0)) := Node []]) c

-- | Descend into selection, if only one element is selected
descend :: EditT Maybe a ann
descend = localEdit $ \ (T ((a, Select (start, end)) := e)) ->
  if abs (start - end) == 1
  then return $ T $ (a, Descend $ min start end) := e
  else mzero

-- | Create new node, edit at begining of it
push :: Num ann => EditM Text ann
push = insertNode >=> mapEdit (Identity . fromJust) descend

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

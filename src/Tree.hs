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
  , Fresh

  , Edit
  , EditT
  , EditM
  , mapEdit
  , justEdit
  , maybeEdit
  , getFresh
  , path

  , elimIsSequence
  , mapIsSequence

  , change
  , insertNode
  , ascend
  , descend
  , push
  , pop

  , switchBounds
  , startMin
  , endMax
  , selectAll
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

class Fresh a where
  fresh :: a -> a

instance Fresh Word where
  fresh = (+ 1)

mapEdit :: (m (Cursor a ann, ann) -> n (Cursor a ann, ann))
         -> EditT m a ann -> EditT n a ann
mapEdit = fmap . mapStateT

justEdit :: EditM a ann -> EditT Maybe a ann
justEdit = mapEdit $ Just . runIdentity

maybeEdit :: EditT Maybe a ann -> EditM a ann
maybeEdit e c = do
  ann <- get
  case runStateT (e c) ann of
    Nothing -> return c
    Just (c', ann') -> put ann' >> return c'

getFresh :: Fresh i => State i i
getFresh = do
  i <- get
  put $ fresh i
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

mapIsSequence :: forall seq ann
              .  IsSequence seq
              => (forall s. IsSequence s => s -> s)
              -> Element seq ann
              -> Element seq ann
mapIsSequence f = \case
  Atom a -> Atom $ f a
  Node s -> Node $ f s

localEdit :: Monad m => EditT m a ann -> EditT m a ann
localEdit f t@(T ((a, sel) := e)) =
  case (sel, e) of
    (Descend _, Atom _)  -> error "path is too deep"
    (Descend i, Node ts) -> do
      child <- localEdit f $ S.index ts i
      return $ T $ (a, sel) := Node (S.update i child ts)
    (Select range, _) -> f t

localMove :: IsSequence a => (Int -> Range -> Range) -> EditT Maybe a ann
localMove f = localEdit $ \ (T ((a, Select r) := e)) ->
  let len = elimIsSequence olength e
      (start', end') = f len r
  in if min start' end' < 0 || max start' end' > len
  then mzero
  else return $ T $ (a, Select (start', end')) := e

change :: forall ann atom
       .  Fresh ann
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
      id <- getFresh
      insert o [T $ (id, Select (0, 0)) := new] Node
    (Atom o, Node n) -> do
      lId <- getFresh
      rId <- getFresh
      let cs = [T $ init lId := Atom lPart] >< n >< [T $ init rId := Atom rPart]
      return $ T $ (a, Select (start', end')) := Node cs
        where (lPart, rPart) = split o
              (start', end') =
                if start <= end
                then (1, 1 + S.length n)
                else (1 + S.length n, 1)

    where init ann = (ann, Select (0, 0))

-- | Insert a new empty node at the cursor
insertNode :: (IsSequence a, Fresh ann) => EditM a ann
insertNode c = do
  id <- getFresh
  change (Node [T $ (id, Select (0, 0)) := Node []]) c

-- | Select the node which we're currently inside
ascend :: EditT Maybe a ann
ascend (T ((a, Select _) := _)) = mzero
ascend (T ((a, Descend i) := Node cs)) =
  case S.index cs i of
    T ((a, Select _) := _) -> return $ T $ (a, Select (i, i + 1)) := Node cs
    t@(T ((a, Descend _) := _)) -> do
      sub <- ascend t
      return $ T $ (a, Descend i) := Node (update i sub cs)

-- | Descend into selection, if only one element is selected
descend :: EditT Maybe a ann
descend = localEdit $ \ (T ((a, Select (start, end)) := e)) ->
  if abs (start - end) == 1 && isNode e
  then return $ T $ (a, Descend $ min start end) := e
  else mzero
  where isNode (Node _) = True
        isNode _        = False


-- * Derived Edits

-- | Create new node, edit at begining of it
push :: (IsSequence a, Fresh ann) => EditM a ann
push = insertNode >=> mapEdit (Identity . fromJust) descend

-- | Go back to editing parent, right of current position
pop :: IsSequence a => EditT Maybe a ann
pop = ascend >=> selectNoneEnd

switchBounds :: IsSequence a => EditT Maybe a ann
switchBounds = localMove $ \ _ (start, end) -> (end, start)

startMin :: IsSequence a => EditT Maybe a ann
startMin = localMove $ \ _ (_, end) -> (0, end)

endMax :: IsSequence a => EditT Maybe a ann
endMax = localMove $ \ size (start, _) -> (start, size)

selectAll :: IsSequence a => EditT Maybe a ann
selectAll = localMove $ \ size (_, end) -> (0, size)

selectNoneStart :: IsSequence a => EditT Maybe a ann
selectNoneStart = localMove $ \ _ (start, _) -> (start, start)

selectNoneEnd :: IsSequence a => EditT Maybe a ann
selectNoneEnd = localMove $ \ _ (_, end) -> (end, end)

shiftLeft :: IsSequence a => EditT Maybe a ann
shiftLeft = localMove $ \ _ (start, end) -> (start - 1, end - 1)

shiftRight :: IsSequence a => EditT Maybe a ann
shiftRight = localMove $ \ _ (start, end) -> (start + 1, end + 1)

moveLeft :: IsSequence a => EditT Maybe a ann
moveLeft = localMove $ \ _ (start, end) -> (start, end - 1)

moveRight :: IsSequence a => EditT Maybe a ann
moveRight = localMove $ \ _ (start, end) -> (start, end + 1)

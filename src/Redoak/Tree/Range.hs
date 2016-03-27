{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Redoak.Tree.Range
  ( Range
  , Selection(..)
  , Path
  , Cursor

  , EditT
  , Edit
  , MaybeEditT
  , MaybeEdit

  , ann
  , justEdit
  , maybeEdit
  , tryEdit
  , getFresh
  , path

  , elimIsSequence
  , mapIsSequence

  , initAnn
  , clearAnn
  , unCursor
  , initCursor
  , getSelection
  , isEmpty
  , isInAtom

  , delete
  , change
  , insertNode
  , Redoak.Tree.Range.reverse
  , ascend
  , descend
  , wrap
  , Redoak.Tree.Range.unwrap
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

import           Control.Comonad.Cofree
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Functor.Identity
import           Data.Maybe as M
import           Data.MonoTraversable hiding (Element)
import           Data.Monoid
import qualified Data.Sequence as S
import           Data.Sequence hiding ((:<))
import           Data.Sequences as SS
import           Data.Word

import           Redoak.Tree


type Range = (Int, Int)

data Selection
  = Descend Int
  | Select Range
  deriving (Eq, Ord, Show)

type Path = ([Int], Range)

type Cursor a ann = Tree a (ann, Selection)

type EditT m a ann r = StateT (Cursor a ann) m r
type Edit a ann r = EditT Identity a ann r

type MaybeEditT m a ann r = EditT (MaybeT m) a ann r
type MaybeEdit a ann r = MaybeEditT Identity a ann r


justEdit :: Monad m => EditT m a ann r -> MaybeEditT m a ann r
justEdit = mapStateT (MaybeT . fmap Just)

maybeEdit :: Monad m
          => MaybeEditT m a ann r
          -> EditT m a ann r
          -> EditT m a ann r
maybeEdit try catch = do
  c <- get
  r <- lift $ runMaybeT $ runStateT try c
  case r of
    Nothing -> catch
    Just (v, c') -> put c' >> return v

tryEdit :: Monad m => MaybeEditT m a ann () -> EditT m a ann ()
tryEdit = flip maybeEdit $ return ()

assumeMaybeEdit :: Monad m => EditT (MaybeT m) a ann r -> EditT m a ann r
assumeMaybeEdit = mapStateT $ fmap fromJust . runMaybeT

path :: Cursor a ann -> Path
path ((_, Descend i) :< Node cs) = first (i :) $ path $ S.index cs i
path ((_, Select r) :< _) = ([], r)

local :: Monad m => EditT m a ann r -> EditT m a ann r
local f = do
  t@((a, sel) :< e) <- get
  case (sel, e) of
    (Descend _, Atom _)  -> error "path is too deep"
    (Descend i, Node ts) -> do
      (r, child) <- lift $ runStateT (local f) $ S.index ts i
      put $ (a, sel) :< Node (S.update i child ts)
      return r
    (Select range, _) -> f

localMove :: (IsSequence a, Monad m) => (Int -> Range -> Range) -> MaybeEditT m a ann ()
localMove f = local $ do
  (a, Select r) :< e <- get
  let len = elimIsSequence olength e
      (start', end') = f len r
  if min start' end' < 0 || max start' end' > len
  then mzero
  else put $ (a, Select (start', end')) :< e

unCursor :: Cursor a ann -> Tree a ann
unCursor = fmap fst

initCursor :: Tree a ann -> Cursor a ann
initCursor = fmap (, Select (0, 0))

isEmpty :: (IsSequence a, Monad m) => EditT m a ann Bool
isEmpty = local $ do
  (_, Select (start, end)) :< e <- get
  return $ start == end

isInAtom :: (IsSequence a, Monad m) => EditT m a ann Bool
isInAtom = local $ do
  _ :< e <- get
  return $ case e of
    Atom _ -> True
    Node _ -> False

getSelection :: (IsSequence a, Monad m) => EditT m a ann (Trunk a (ann, Selection))
getSelection = local $ do
  (_, Select r) :< e <- get
  return $ mapIsSequence (getRange r) e
  where getRange :: IsSequence s => Range -> s -> s
        getRange (start, end) =
          SS.take (fromIntegral $ abs $ start - end) .
          SS.drop (fromIntegral $ min start end)

-- TODO: overlap between delete and change

delete :: forall m ann atom
      .  Fresh ann
      => IsSequence atom
      => Monad m
      => EditT (StateT ann m) atom ann ()
delete = local $ do
  (a, Select (start, end)) :< sel <- get
  let front = min start end
  let back  = max start end
  let f seq = lpart <> rpart
        where lpart = SS.take (fromIntegral front) seq
              rpart = SS.drop (fromIntegral back)  seq
  put $ ((a, Select (front, front)) :<) $ mapIsSequence f sel

change :: forall m ann atom
       .  Fresh ann
       => IsSequence atom
       => Monad m
       => Trunk atom (ann, Selection)
       -> EditT (StateT ann m) atom ann ()
change new = local $ do
    (a, Select (start, end)) :< old <- get

    let insert :: forall seq. IsSequence seq
               => seq
               -> seq
               -> (seq -> Trunk atom (ann, Selection))
               -> EditT (StateT ann m) atom ann ()
        insert old new inj =
          put $ (a, Select $ adjustRange new) :< inj seq'
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

    case (old, new) of
      -- homogenous
      (Atom o, Atom n) -> insert o n Atom
      (Node o, Node n) -> insert o n Node
      -- heterogenous
      (Node o, Atom _) -> do
        id <- lift getFresh
        insert o [(id, Select (0, 0)) :< new] Node
      (Atom o, Node n) -> do
        lId <- lift getFresh
        rId <- lift getFresh
        let cs = [init lId :< Atom lPart] >< n >< [init rId :< Atom rPart]
        put $ (a, Select (start', end')) :< Node cs
          where (lPart, rPart) = split o
                (start', end') =
                  if start <= end
                  then (1, 1 + S.length n)
                  else (1 + S.length n, 1)

    where init ann = (ann, Select (0, 0))

-- | Insert a new empty node at the cursor
insertNode :: (IsSequence a, Fresh ann, Monad m)
           => EditT (StateT ann m) a ann ()
insertNode = do
  id <- lift getFresh
  change (Node [(id, Select (0, 0)) :< Node []])

-- | Select the node which we're currently inside
ascend :: Monad m => MaybeEditT m a ann ()
ascend = (get >>=) $ \case
  (a, Select _) :< _ -> mzero
  (a, Descend i) :< Node cs ->
    case S.index cs i of
      (_, Select _) :< _ -> put $ (a, Select (i, i + 1)) :< Node cs
      t@((_, Descend _) :< _) -> do
        sub <- lift $ execStateT ascend t
        put $ (a, Descend i) :< Node (update i sub cs)

-- | Descend into selection, if only one element is selected
descend :: Monad m => EditT (MaybeT m) a ann ()
descend = local $ do
  (a, Select (start, end)) :< e <- get
  if abs (start - end) == 1 && isNode e
  then put $ (a, Descend $ min start end) :< e
  else mzero
  where isNode (Node _) = True
        isNode _        = False

reverse :: (IsSequence a, Fresh ann, Monad m) => EditT (StateT ann m) a ann ()
reverse = local $ getSelection >>= (change . mapIsSequence SS.reverse)

-- * Derived Edits

wrap :: (IsSequence a, Fresh ann, Monad m) => EditT (StateT ann m) a ann ()
wrap = do
  sel <- getSelection
  push
  change sel

unwrap :: (IsSequence a, Fresh ann, Monad m) => MaybeEditT (StateT ann m) a ann ()
unwrap = do
  sel <- getSelection
  ascend
  justEdit $ change sel

-- | Create new node, edit at begining of it
push :: (IsSequence a, Fresh ann, Monad m) => EditT (StateT ann m) a ann ()
push = insertNode >> assumeMaybeEdit descend

-- | Go back to editing parent, right of current position
pop :: (IsSequence a, Monad m) => MaybeEditT m a ann ()
pop = ascend >> selectNoneEnd

switchBounds :: (IsSequence a, Monad m) => MaybeEditT m a ann ()
switchBounds = localMove $ \ _ (start, end) -> (end, start)

startMin :: (IsSequence a, Monad m) => MaybeEditT m a ann ()
startMin = localMove $ \ _ (_, end) -> (0, end)

endMax :: (IsSequence a, Monad m) => MaybeEditT m a ann ()
endMax = localMove $ \ size (start, _) -> (start, size)

selectAll :: (IsSequence a, Monad m) => MaybeEditT m a ann ()
selectAll = localMove $ \ size (_, end) -> (0, size)

selectNoneStart :: (IsSequence a, Monad m) => MaybeEditT m a ann ()
selectNoneStart = localMove $ \ _ (start, _) -> (start, start)

selectNoneEnd :: (IsSequence a, Monad m) => MaybeEditT m a ann ()
selectNoneEnd = localMove $ \ _ (_, end) -> (end, end)

shiftLeft :: (IsSequence a, Monad m) => MaybeEditT m a ann ()
shiftLeft = localMove $ \ _ (start, end) -> (start - 1, end - 1)

shiftRight :: (IsSequence a, Monad m) => MaybeEditT m a ann ()
shiftRight = localMove $ \ _ (start, end) -> (start + 1, end + 1)

moveLeft :: (IsSequence a, Monad m) => MaybeEditT m a ann ()
moveLeft = localMove $ \ _ (start, end) -> (start, end - 1)

moveRight :: (IsSequence a, Monad m) => MaybeEditT m a ann ()
moveRight = localMove $ \ _ (start, end) -> (start, end + 1)

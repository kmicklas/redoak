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

import           Control.Comonad.Cofree8

import           Redoak.Language hiding ( Range, Selection(..), Path, Cursor, EditT
                                        , Edit, MaybeEditT, MaybeEdit
                                        , justEdit
                                        , maybeEdit
                                        , tryEdit
                                        , assumeMaybeEdit
                                        , local)
import           Redoak.Language.Fundamental


type Range n = (n, n)

data Selection
  = Descend Word
  | Select (Range Word)
  deriving (Eq, Ord, Show)

type Path = ([Word], Range Word)

type Cursor a ann = Tree a (ann, Selection)

type EditT m a ann r = StateT (Cursor a ann) m r
type Edit a ann r = EditT Identity a ann r

type MaybeEditT m a ann r = EditT (MaybeT m) a ann r
type MaybeEdit a ann r = MaybeEditT Identity a ann r

justEdit :: Monad m
         => EditT m a ann r
         -> MaybeEditT m a ann r
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

tryEdit :: Monad m
        => MaybeEditT m a ann ()
        -> EditT m a ann ()
tryEdit = flip maybeEdit $ return ()

assumeMaybeEdit :: Monad m
                => MaybeEditT m a ann r
                -> EditT m a ann r
assumeMaybeEdit = mapStateT $ fmap fromJust . runMaybeT

local :: Monad m => EditT m a ann r -> EditT m a ann r
local f = do
  t@((a, sel) :< e) <- get
  case (sel, e) of
    (Descend _, Atom _)  -> error "path is too deep"
    (Descend i, Node ts) -> do
      (r, child) <- lift $ runStateT (local f) $ indexSW ts i
      put $ (a, sel) :< Node (S.update (fromIntegral i) child ts)
      return r
    (Select range, _) -> f

--
---
--
--
---
---
----


indexSW :: Seq a -> Word -> a
indexSW cs i = S.index cs $ fromIntegral i

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
  where getRange :: IsSequence s => Range Word -> s -> s
        getRange (start, end) =
          SS.take (fromIntegral $ diff start end) .
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

        adjustRange :: forall seq. IsSequence seq => seq -> Range Word
        adjustRange new = if start <= end
                          then (start, start + len)
                          else (end + len, end)
          where len = fromIntegral (olength new)

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
                len = fromIntegral $ S.length n
                (start', end') =
                  if start <= end
                  then (1, 1 + len)
                  else (1 + len, 1)

    where init ann = (ann, Select (0, 0))

-- | Insert a new empty node at the cursor
insertNode :: (IsSequence a, Fresh ann, Monad m)
           => EditT (StateT ann m) a ann ()
insertNode = do
  id <- lift getFresh
  change (Node [(id, Select (0, 0)) :< Node []])

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
  _ascend
  justEdit $ change sel

-- | Create new node, edit at begining of it
push :: (IsSequence a, Fresh ann, Monad m) => EditT (StateT ann m) a ann ()
push = insertNode >> assumeMaybeEdit _descend

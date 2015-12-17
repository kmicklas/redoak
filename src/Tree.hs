{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Tree
  ( Cofree(..)
  , Tree
  , Trunk
  , Element(..)
  , Range
  , Selection(..)
  , Path
  , Cursor
  , Fresh

  , EditT
  , Edit
  , MaybeEditT
  , MaybeEdit

  , ann
  , justEdit
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

  , delete
  , change
  , insertNode
  , Tree.reverse
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

import           Control.Comonad.Cofree
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Maybe

import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bifunctor.TH
import           Data.Bitraversable
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Monoid
import           Data.Traversable
import           Data.MonoTraversable hiding (Element)
import qualified Data.List as L
import           Data.Maybe as M
import           Data.Sequence hiding ((:<))
import qualified Data.Sequence as S
import           Data.Sequences as SS
import           Data.Word

data Element a b
  = Atom { value :: a }
  | Node { children :: Seq b }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
deriveBifunctor ''Element
deriveBifoldable ''Element
deriveBitraversable ''Element

type Tree a ann = Cofree (Element a) ann

-- | A Trunk is the unidentified part of a Tree
type Trunk a ann = Element a (Tree a ann)

newtype T a ann = T { unT :: (Tree a ann) }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor T where
  first f (T (a :< e)) = T $ (a :<) $ case e of
    Atom v -> Atom $ f v
    Node ts -> Node $ (unT . first f . T) <$> ts
  second = fmap

instance Bifoldable T where
  bifoldr f g z (T (a :< e)) = case e of
    Atom v -> f v z
    Node ts -> foldr (flip (bifoldr f g) . T) z ts

instance Bitraversable T where
  bitraverse f g (T (a :< e)) = (T <$>) $ (((:<) <$> g a) <*>) $ case e of
    Atom v -> Atom <$> f v
    Node ts -> Node <$> traverse ((unT <$>) . bitraverse f g . T) ts

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

class Fresh a where
  fresh :: a -> a

instance Fresh Word where
  fresh = (+ 1)

getFresh :: (Fresh a, Monad m) => StateT a m a
getFresh = do
  i <- get
  put $ fresh i
  return i

ann :: Tree a ann -> ann
ann (a :< _) = a

justEdit :: Monad m => EditT m a ann r -> MaybeEditT m a ann r
justEdit = mapStateT (MaybeT . fmap Just)

tryEdit :: Monad m => MaybeEditT m a ann () -> EditT m a ann ()
tryEdit e = do
  c <- get
  r <- lift $ runMaybeT $ execStateT e c
  case r of
    Nothing -> return ()
    Just c' -> put c'

assumeMaybeEdit :: Monad m => EditT (MaybeT m) a ann r -> EditT m a ann r
assumeMaybeEdit = mapStateT $ fmap fromJust . runMaybeT

path :: Cursor a ann -> Path
path ((_, Descend i) :< Node cs) = first (i :) $ path $ S.index cs i
path ((_, Select r) :< _) = ([], r)

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

clearAnn :: Tree a ann -> Tree a ()
clearAnn = unT . second (const ()) . T

initAnn :: (Fresh ann, Monad m) => Tree a old -> StateT ann m (Tree a ann)
initAnn = (unT <$>) . bitraverse pure (const getFresh) . T

getSelection :: (IsSequence a, Monad m) => EditT m a ann (Trunk a (ann, Selection))
getSelection = local $ do
  (_, Select r) :< e <- get
  case e of
    Atom a -> return $ Atom $ getRange r a
    Node ts -> return $ Node $ getRange r ts

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
  let f seq = lpart <> rpart
        where lpart = SS.take (fromIntegral $ min start end) seq
              rpart = SS.drop (fromIntegral $ max start end) seq
  put $ ((a, Select (start, start)) :<) $ mapIsSequence f sel

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

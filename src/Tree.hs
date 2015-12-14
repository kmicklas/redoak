{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}

module Tree
  ( Tree(..)
  , Ann(..)
  , Element(..)
  , Range
  , Path(..)
  , Cursor(..)

  , Edit
  , EditT
  , EditM
  , mapEdit
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

type Trunk a ann = (Element a (Tree a ann))

newtype Tree a ann
  = T { unTree :: Ann (Trunk a ann) ann }
  deriving (Eq, Ord, Show)

type Range = (Int, Int)

data Path
  = Int :\/ Path
  | Select Range
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

mapEdit :: (m (Cursor a ann, ann)
         -> n (Cursor a ann, ann))
         -> EditT m a ann -> EditT n a ann
mapEdit = fmap . mapStateT

freshId :: Num i => State i i
freshId = do
  i <- get
  put $ i + 1
  return i

elimIsSequence :: forall seq x ret
               .  IsSequence seq
               => (forall s. IsSequence s => s -> ret)
               -> Element seq x
               -> ret
elimIsSequence f = \case
  Atom a -> f a
  Node s -> f s

localEdit :: Monad m
          => ((Trunk a ann, Range) -> StateT ann m (Trunk a ann, Path))
          -> EditT m a ann
localEdit f (Cursor (T (a := e)) path) = do
  (e', path') <- case (path, e) of
    (_ :\/ _,  Atom  _) -> error "path is too deep"
    (i :\/ is, Node ts) -> do
      let child = S.index ts i
      (Cursor child' is') <- localEdit f $ Cursor child is

      return ( Node $ S.update i child' ts
             , i :\/ is')
    (Select range, _) -> f (e, range)

  return $ Cursor (T $ a := e') path'

localMove :: (Int -> Range -> Range) -> EditM Text ann
localMove f = localEdit $ \(t, r) -> return $ (t, Select $ f (elimIsSequence olength t) r)

change :: forall ann atom
       .  Num ann
       => IsSequence atom
       => Trunk atom ann
       -> EditM atom ann
change new = localEdit $ \(old, (start, end)) -> let
    f :: forall seq. IsSequence seq
      => seq
      -> seq
      -> (seq -> Element atom (Tree atom ann))
      -> StateT ann Identity (Trunk atom ann, Path)
    f old new inj = return $ (inj seq', Select $ h new)
      where seq' = mconcat [lPart, new, rPart]
            (lPart, rPart) = g old

    g :: forall seq. IsSequence seq => seq -> (seq, seq)
    g old = ( SS.take (fromIntegral $ min start end) old
            , SS.drop (fromIntegral $ max start end) old)

    h :: forall seq.IsSequence seq => seq -> (Int, Int)
    h new = if start <= end
            then (start, start + olength new)
            else (end + olength new, end)

  in case (old, new) of
    -- homogenous
    (Atom o, Atom n) -> f o n Atom
    (Node o, Node n) -> f o n Node
    -- heterogenous
    (Node o, Atom _) -> do
      id <- freshId
      f o [T $ id := new] Node
    (Atom o, Node n) -> do
      lId <- freshId
      rId <- freshId
      let cs' = [T $ lId := Atom lPart] >< n >< [T $ rId := Atom rPart]
      return $ (Node cs', Select (start', end'))
        where (lPart, rPart) = g o
              (start', end') = h n

-- | Go back to editing parent, right of current position
-- | new parent if at root
pop :: EditT Maybe a ann
pop (Cursor t p) = Cursor t <$> f p
  where f = \case
          oops@(Select _)  -> mzero
          (i :\/ Select _) -> return $ Select (i + 1, i + 1)
          (i :\/ is)       -> (i :\/) <$> f is

-- | Create new node, edit at begining of it
push :: Num ann => EditM Text ann
push c = do
  id <- freshId
  Cursor t p <- change (Node [T $ id := Node []]) c
  return $ Cursor t $ f p
  where f = \case
          Select (start, end) -> min start end :\/ Select (0, 0)
          i :\/ is            -> i :\/ f is

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

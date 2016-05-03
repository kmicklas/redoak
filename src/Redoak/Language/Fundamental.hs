{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Redoak.Language.Fundamental where

import Control.Lens hiding ((:<))
import Control.Lens.TH
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Bifoldable
import Data.Bifunctor
import Data.Bifunctor.TH
import Data.Bitraversable
import Data.Coerce
import Data.Map
import Data.MonoTraversable hiding (Element)
import Data.Monoid
import Data.Sequence as S hiding ((:<))
import Data.Sequences as SS
import Data.Void

import Control.Comonad.Cofree8
import Data.Functor8
import Data.Foldable8
import Data.Traversable8

import Redoak.Language
import Redoak.Language.Empty


data Element a b
  = Atom { _value :: a }
  | Node { _children :: Seq b }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
makeLenses ''Element
deriveBifunctor ''Element
deriveBifoldable ''Element
deriveBitraversable ''Element


newtype LiftBf8 bf a a0 a1 a2 a3 a4 a5 a6 a7 = LiftBf8 { _lowerBf8 :: bf a a7 }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
makeLenses ''LiftBf8

instance Bifunctor f => Functor8 (LiftBf8 f a) where
  map8 _ _ _ _ _ _ _ f = lowerBf8 %~ second f

instance Bifoldable f => Foldable8 (LiftBf8 f a) where
  foldMap8 _ _ _ _ _ _ _ f = bifoldMap (const mempty) f . _lowerBf8

instance Bitraversable f => Traversable8 (LiftBf8 f a) where
  traverse8 _ _ _ _ _ _ _ f = lowerBf8 `traverseOf` bitraverse pure f


type MkBfTree bf a index ann =
  Cofree8'
    Void8 Void8 Void8 Void8 Void8 Void8 Void8 (LiftBf8 bf a)
    index
    ann

pattern (:<) :: ann -> Element a (Tree a ann) -> Tree a ann
pattern a :< b = CF7 a (LiftBf8 b)

type MkTree a index ann = MkBfTree Element a index ann

type Tree a ann = MkTree a 7 ann

-- | A Trunk is the unidentified part of a Tree
type Trunk a ann = Element a (MkTree a 7 ann)

newtype Cofree8Bifunctor bf a ann =
  Cofree8Bifunctor { _unCofree8Bifunctor :: MkBfTree bf a 7 ann }
makeLenses ''Cofree8Bifunctor

instance Bifunctor bf => Bifunctor (Cofree8Bifunctor bf) where
  bimap f g = unCofree8Bifunctor %~ go where
    go (CF7 a e) = g a `CF7` (lowerBf8 %~ bimap f go) e

instance Bifoldable bf => Bifoldable (Cofree8Bifunctor bf) where
  bifoldMap f g = go . _unCofree8Bifunctor where
    go (CF7 a e) = g a `mappend` bifoldMap f go (_lowerBf8 e)

instance Bitraversable bf => Bitraversable (Cofree8Bifunctor bf)  where
  bitraverse f g = unCofree8Bifunctor `traverseOf` go where
    go (CF7 a e) = CF7 <$> g a <*> (lowerBf8 `traverseOf` bitraverse f go) e


elimIsSequence :: forall a n ret
               .  IsSequence a
               => (forall s. IsSequence s => s -> ret)
               -> Element a n
               -> ret
elimIsSequence f = \case
  Atom a -> f a
  Node s -> f s


mapIsSequence :: forall a n
              .  IsSequence a
              => (forall s. IsSequence s => s -> s)
              -> Element a n
              -> Element a n
mapIsSequence f = \case
  Atom a -> Atom $ f a


instance IsSequence a => NonTerminal (LiftBf8 Element a) where
  length (LiftBf8 e) = fromIntegral $ elimIsSequence olength e

  introductions = Data.Map.empty

  canSelectRange _ = True

  canDescend (LiftBf8 e) = case e of
    Atom _ -> False
    Node _ -> True

  indexC (LiftBf8 e) i _ _ _ _ _ _ _ k = case e of
    Atom _ -> undefined
    Node s -> k $ S.index s (fromIntegral i)

  modifyC (LiftBf8 e) i _ _ _ _ _ _ _ k = case e of
    Atom _ -> undefined
    Node s -> LiftBf8 <$> Node <$> flip (S.update i') s <$> k (S.index s i')
      where i' = fromIntegral i

instance IsSequence a
         => Language Void8 Void8 Void8 Void8 Void8 Void8 Void8 (LiftBf8 Element a) where

type Cursor' a ann = Tree a (ann, Selection)

type EditT' m a ann r = StateT (Cursor' a ann) m r
type Edit' a ann r = EditT' Identity a ann r
type MaybeEditT' m a ann r = EditT' (MaybeT m) a ann r
type MaybeEdit' m a ann r = MaybeEditT' Identity a ann r

  
isInAtom :: (IsSequence a, Monad m) => EditT' m a ann Bool
isInAtom = local $ do
  _ `CF7` (LiftBf8 e) <- get
  return $ case e of
    Atom _ -> True
    Node _ -> False


getSelection :: (IsSequence a, Monad m) => EditT' m a ann (Trunk a (ann, Selection))
getSelection = local $ do
  (_, Select (Range r)) `CF7` (LiftBf8 e) <- get
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
       => EditT' (StateT ann m) atom ann ()
delete = local $ do
  (a, Select (Range (start, end))) `CF7` (LiftBf8 sel) <- get
  let front = min start end
  let back  = max start end
  let f :: forall s. IsSequence s => s -> s
      f seq = lpart <> rpart
        where lpart = SS.take (fromIntegral front) seq
              rpart = SS.drop (fromIntegral back)  seq
  put $ ((a, Select (Range (front, front))) :<) $ mapIsSequence f sel

change :: forall m ann atom
       .  Fresh ann
       => IsSequence atom
       => Monad m
       => Trunk atom (ann, Selection)
       -> EditT' (StateT ann m) atom ann ()
change new = local $ do
    (a, Select (Range (start, end))) `CF7` (LiftBf8 old) <- get

    let insert :: forall seq. IsSequence seq
               => seq
               -> seq
               -> (seq -> Trunk atom (ann, Selection))
               -> EditT' (StateT ann m) atom ann ()
        insert old new inj =
          put $ (a, Select $ Range $ adjustRange new) :< inj seq'
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
        insert o [(id, Select (Range (0, 0))) :< new] Node
      (Atom o, Node n) -> do
        lId <- lift getFresh
        rId <- lift getFresh
        let cs = [init lId :< Atom lPart] >< n >< [init rId :< Atom rPart]
        put $ (a, Select (Range (start', end'))) :< Node cs
          where (lPart, rPart) = split o
                len = fromIntegral $ S.length n
                (start', end') =
                  if start <= end
                  then (1, 1 + len)
                  else (1 + len, 1)

    where init ann = (ann, Select $ Range (0, 0))

-- | Insert a new empty node at the cursor
insertNode :: (IsSequence a, Fresh ann, Monad m)
           => EditT' (StateT ann m) a ann ()
insertNode = do
  id <- lift getFresh
  change (Node [(id, Select $ Range (0, 0)) :< Node []])

reverse :: (IsSequence a, Fresh ann, Monad m) => EditT' (StateT ann m) a ann ()
reverse = local $ do
  _ `CF7` (LiftBf8 _) <- get -- desugared pattern match needed for absurd pattern
  getSelection >>= (change . mapIsSequence SS.reverse)

-- * Derived Edits

wrap :: (IsSequence a, Fresh ann, Monad m) => EditT' (StateT ann m) a ann ()
wrap = do
  sel <- getSelection
  push
  change sel

unwrap :: (IsSequence a, Fresh ann, Monad m) => MaybeEditT' (StateT ann m) a ann ()
unwrap = do
  sel <- getSelection
  ascend
  justEdit $ change sel

-- | Create new node, edit at begining of it
push :: (IsSequence a, Fresh ann, Monad m) => EditT' (StateT ann m) a ann ()
push = insertNode >> assumeMaybeEdit descend

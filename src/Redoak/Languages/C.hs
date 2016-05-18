{-# LANGUAGE OverloadedStrings #-}
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

module Redoak.Languages.C where

import Control.Lens
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Coerce
import Data.Map (Map, fromList, empty)
import Data.Maybe
import Data.Monoid
import Data.Sequence as S hiding ((:<))
import Data.Text as T hiding (copy)

import Control.Comonad.Cofree8
import Data.Functor8
import Data.Foldable8
import Data.Traversable8

import Redoak.Event
import Redoak.Language
import Redoak.Language.Hole
import Redoak.Language.DefaultInput
import Redoak.Languages.Empty
import Redoak.Languages.Fundamental hiding ( Trunk
                                           , Ann', Ann'', Accum', AccumP, Accum''
                                           , Editor, Mode(..))



newtype Items tyIdents ident tyIdent ty exp block item items
  = Items { _items :: Seq item }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
makeLenses ''Items

data NominalSort = Struct | Union
  deriving (Eq, Ord, Show)

data Item tyIdents ident tyIdent ty exp block item items
  = Nominal NominalSort ident tyIdents
  | Function            ident tyIdents block
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Block tyIdents ident tyIdent ty exp block item items
  = Block { _block :: Seq exp }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
makeLenses ''Block

data PrimMonOpSort
  = Ref | Deref
  | BoolInv | BinInv
  deriving (Eq, Ord, Show)

data PrimBinOpSort
  = Add | Sub | Mult | Divide
  | BoolOr | Booland
  | BinOr  | BinAnd | BinXor
  deriving (Eq, Ord, Show)

data Expr tyIdents ident tyIdent ty exp block item items
  = Ident ident
  | Decl tyIdent exp
  | PrimMonOp PrimMonOpSort exp
  | PrimBinOp PrimBinOpSort exp exp
  | App exp (Seq exp)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Signage = Signed | Unsigned
  deriving (Eq, Ord, Show)

data NumType = Char | Short | Int | Long
  deriving (Eq, Ord, Show)

data Type tyIdents ident tyIdent ty exp block item items
  = Void
  | NumType Signage NumType
  | NominalType NominalSort ident
  | Ptr ty
  | FunPtr ty (Seq ty)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype TyIdents tyIdents ident tyIdent ty exp block item items
  = TyIdents { _tyIdents :: (Seq tyIdent) }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
makeLenses ''TyIdents

data TyIdent tyIdents ident tyIdent ty exp block item items
  = TyIdent ty ident
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Ident tyIdents ident tyIdent ty exp block item items
  = Text Text
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type RawC n ann = Cursor         TyIdents Ident TyIdent Type Expr Block Item Items n ann
type C    n ann = CursorWithHole TyIdents Ident TyIdent Type Expr Block Item Items n ann



instance Functor8 Items where
  map8 _ _ _ _ _ _ i _ = items %~ fmap i

instance Functor8 Item where
  map8 tis i ti t e b it its = \case
    Nominal nomSort ident tyIdents       -> Nominal nomSort (i ident) (tis tyIdents)
    Function        ident tyIdents block -> Function (i ident) (tis tyIdents) (b block)

instance Functor8 Expr where
  map8 _ i ti t e b it its = \case
    Ident ident          -> Ident (i ident)
    Decl tyIdent exp     -> Decl (ti tyIdent) (e exp)
    PrimMonOp sort e0    -> PrimMonOp sort (e e0)
    PrimBinOp sort e0 e1 -> PrimBinOp sort (e e0) (e e1)
    App e0 es            -> App (e e0) (e <$> es)

instance Functor8 Type where
  map8 _ i _ t _ _ _ _ = \case
    Void             -> Void
    NumType s nt     -> NumType s nt
    NominalType s i0 -> NominalType s (i i0)
    Ptr t0           -> Ptr (t t0)
    FunPtr t0 ts     -> FunPtr (t t0) (t <$> ts)

instance Functor8 TyIdents where
  map8 _ _ ti _ _ _ _ _ = tyIdents %~ fmap ti

instance Functor8 TyIdent where
  map8 _ i _ t _ _ _ _ (TyIdent t0 i0) = TyIdent (t t0) (i i0)

instance Functor8 Ident where
  map8 _ _ _ _ _ _ _ _ = coerce



instance NonTerminal Items where
  length = fromIntegral . S.length . _items
  canSelectRange _ = True
  canDescend _ = True
  indexC  (Items s) i _ _ _ _ _ _ k _ = k $ S.index s $ fromIntegral i
  modifyC (Items s) i _ _ _ _ _ _ k _ = Items <$> flip (S.update i') s <$> k (S.index s i')
    where i' = fromIntegral i

instance NonTerminal Item where
  length = \case
    Nominal _ _ _ -> 2
    Function _ _ _ -> 3
  canSelectRange _ = False
  canDescend _ = True
  indexC  it ix _ _ _ _ _ _ k _ = case it of
    Nominal _ i0 tis     -> case ix of
      0 -> _
      1 -> _
    Function  i0 tis blk -> case ix of
      0 -> _
      1 -> _
      2 -> _
  modifyC it ix _ _ _ _ _ _ k _ = case it of
    Nominal _ i0 tis     -> case ix of
      0 -> _
      1 -> _
    Function  i0 tis blk -> case ix of
      0 -> _
      1 -> _
      2 -> _
    where i' = fromIntegral ix

instance NonTerminal Block where
  length = fromIntegral . S.length . _block
  canSelectRange _ = True
  canDescend _ = True
  indexC  (Block s) i _ _ _ _ k _ _ _ = k $ S.index s $ fromIntegral i
  modifyC (Block s) i _ _ _ _ k _ _ _ = Block <$> flip (S.update i') s <$> k (S.index s i')
    where i' = fromIntegral i

instance NonTerminal Expr where

instance NonTerminal Type where

instance NonTerminal TyIdents where
  length = fromIntegral . S.length . _tyIdents
  canSelectRange _ = True
  canDescend _ = True
  indexC  (TyIdents s) i _ _ _ _ k _ _ _ = k $ S.index s $ fromIntegral i
  modifyC (TyIdents s) i _ _ _ _ k _ _ _ = TyIdents <$> flip (S.update i') s <$> k (S.index s i')
    where i' = fromIntegral i

instance NonTerminal TyIdent where

instance NonTerminal Ident where



instance Completable Items where

instance Completable Item where

instance Completable Block where

instance Completable Expr where

instance Completable Type where

instance Completable TyIdent where

instance Completable Ident where



instance Language (WithHole Void8) (WithHole Ident) (WithHole TyIdent) (WithHole Type)
                  (WithHole Expr)  (WithHole Block) (WithHole Item)    (WithHole Items) where

  type Ann (WithHole Void8) (WithHole Ident) (WithHole TyIdent) (WithHole Type)
           (WithHole Expr)  (WithHole Block) (WithHole Item)    (WithHole Items) = Ann'

  type Accum (WithHole Void8) (WithHole Ident) (WithHole TyIdent) (WithHole Type)
             (WithHole Expr)  (WithHole Block) (WithHole Item)    (WithHole Items) = Accum'

  handleEvent = runStateOnly . handleEvent'

  getMessage a@(_, s) = ( "C: " <> printMode (_mode s)
                        , renderChoices a)

type AccumP = (Trunk, Accum')

type Term'' n = C n Ann'
type Trunk  = Term'' 7

initState :: AccumP
initState = ( (0, Select $ Range (0, 0)) `CF7` Filled (Items [])
            , Editor
              { _mode = Normal
              , _currentId = 1
              })

instance Renderable
           (WithHole Void8) (WithHole Ident) (WithHole TyIdent) (WithHole Type)
           (WithHole Expr)  (WithHole Block) (WithHole Item)    (WithHole Items)
           Void8 Void8 Void8 Void8 Void8 Void8 Void8 (LiftBf8 Element Text) where
  convert = _

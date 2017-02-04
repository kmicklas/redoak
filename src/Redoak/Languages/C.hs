{-# LANGUAGE OverloadedLists #-}
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
{-# LANGUAGE FlexibleInstances #-}

module Redoak.Languages.C where

import           Control.Lens hiding ((:<))
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Coerce
import           Data.Map as M (Map, fromList, empty, singleton, fromList)
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence as S hiding ((:<))
import           Data.Text as T hiding (copy)

import           Control.Comonad.Cofree8
import           Data.Functor8
import           Data.Foldable8
import           Data.Traversable8

import           Redoak.Event
import           Redoak.Language
import           Redoak.Language.Hole
import           Redoak.Language.DefaultInput
import           Redoak.Languages.Empty
import           Redoak.Languages.Fundamental (pattern (:<))
import           Redoak.Languages.Fundamental hiding ( Trunk
                                                     , Ann', Ann'', Accum', AccumP, Accum''
                                                     , Editor, Mode(..)
                                                     , currentId, _currentId)
import qualified Redoak.Languages.Fundamental as Fundamental



newtype Items ident tyIdent tyIdents ty exp block item items
  = Items { _items :: Seq item }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
makeLenses ''Items

data NominalSort = Struct | Union
  deriving (Eq, Ord, Show)

data Item ident tyIdent tyIdents ty exp block item items
  = Nominal NominalSort ident tyIdents
  | Function            ident tyIdents block
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Block ident tyIdent tyIdents ty exp block item items
  = Block { _block :: Seq exp }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
makeLenses ''Block

data PrimMonOpSort
  = Ref | Deref
  | BoolInv | BinInv
  deriving (Eq, Ord, Show)

data PrimBinOpSort
  = Add | Sub | Mult | Divide
  | BoolOr | BoolAnd
  | BinOr  | BinAnd | BinXor
  deriving (Eq, Ord, Show)

data Expr ident tyIdent tyIdents ty exp block item items
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

data Type ident tyIdent tyIdents ty exp block item items
  = Void
  | NumType Signage NumType
  | NominalType NominalSort ident
  | Ptr ty
  | FunPtr ty (Seq ty)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype TyIdents ident tyIdent tyIdents ty exp block item items
  = TyIdents { _tyIdents :: (Seq tyIdent) }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
makeLenses ''TyIdents

data TyIdent ident tyIdent tyIdents ty exp block item items
  = TyIdent ty ident
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Ident ident tyIdent tyIdents ty exp block item items
  = Text { _text :: Text }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
makeLenses ''Text

type RawC n ann = Cursor         Ident TyIdent TyIdents Type Expr Block Item Items n ann
type C    n ann = CursorWithHole Ident TyIdent TyIdents Type Expr Block Item Items n ann



instance Functor8 Items where
  map8 _ _ _ _ _ _ i _ = items %~ fmap i

instance Functor8 Item where
  map8 i ti tis t e b it its = \case
    Nominal nomSort ident tyIdents       -> Nominal nomSort (i ident) (tis tyIdents)
    Function        ident tyIdents block -> Function        (i ident) (tis tyIdents) (b block)

instance Functor8 Expr where
  map8 i ti _ t e b it its = \case
    Ident ident          -> Ident (i ident)
    Decl tyIdent exp     -> Decl (ti tyIdent) (e exp)
    PrimMonOp sort e0    -> PrimMonOp sort (e e0)
    PrimBinOp sort e0 e1 -> PrimBinOp sort (e e0) (e e1)
    App e0 es            -> App (e e0) (e <$> es)

instance Functor8 Block where
  map8 _ _ _ _ e _ _ _ = block %~ fmap e

instance Functor8 Type where
  map8 i _ _ t _ _ _ _ = \case
    Void             -> Void
    NumType s nt     -> NumType s nt
    NominalType s i0 -> NominalType s (i i0)
    Ptr t0           -> Ptr (t t0)
    FunPtr t0 ts     -> FunPtr (t t0) (t <$> ts)

instance Functor8 TyIdents where
  map8 _ ti _ _ _ _ _ _ = tyIdents %~ fmap ti

instance Functor8 TyIdent where
  map8 i _ _ t _ _ _ _ (TyIdent t0 i0) = TyIdent (t t0) (i i0)

instance Functor8 Ident where
  map8 _ _ _ _ _ _ _ _ = coerce



instance Foldable8 Items where
  foldMap8 _ _ _ _ _ _ i _ = foldMap i . _items

instance Foldable8 Item where
  foldMap8 i ti tis t e b it its = \case
    Nominal _ ident tyIdents       -> (i ident) <> (tis tyIdents)
    Function  ident tyIdents block -> (i ident) <> (tis tyIdents) <> (b block)

instance Foldable8 Block where
  foldMap8 _ _ _ _ e _ _ _ = foldMap e . _block

instance Foldable8 Expr where
  foldMap8 i ti _ t e b it its = \case
    Ident ident          -> i ident
    Decl tyIdent exp     -> (ti tyIdent) <> (e exp)
    PrimMonOp sort e0    -> e e0
    PrimBinOp sort e0 e1 -> (e e0) <> (e e1)
    App e0 es            -> (e e0) <> (foldMap e es)

instance Foldable8 Type where
  foldMap8 i _ _ t _ _ _ _ = \case
    Void             -> mempty
    NumType s nt     -> mempty
    NominalType s i0 -> i i0
    Ptr t0           -> t t0
    FunPtr t0 ts     -> (t t0) <> (foldMap t ts)

instance Foldable8 TyIdents where
  foldMap8 _ ti _ _ _ _ _ _ = foldMap ti ._tyIdents

instance Foldable8 TyIdent where
  foldMap8 i _ _ t _ _ _ _ (TyIdent t0 i0) = (t t0) <> (i i0)

instance Foldable8 Ident where
  foldMap8 _ _ _ _ _ _ _ _ = mempty


instance Traversable8 Items where
  traverse8 _ _ _ _ _ _ i _ = items `traverseOf` traverse i

instance Traversable8 Item where
  traverse8 i ti tis t e b it its = \case
    Nominal nomSort ident tyIdents       -> Nominal nomSort <$> i ident <*> tis tyIdents
    Function        ident tyIdents block -> Function <$> i ident <*> tis tyIdents <*> b block

instance Traversable8 Block where
  traverse8 _ _ _ _ e _ _ _ = block `traverseOf` traverse e

instance Traversable8 Expr where
  traverse8 i ti _ t e b it its = \case
    Ident ident          -> Ident <$> i ident
    Decl tyIdent exp     -> Decl <$> ti tyIdent <*> e exp
    PrimMonOp sort e0    -> PrimMonOp sort <$> e e0
    PrimBinOp sort e0 e1 -> PrimBinOp sort <$> e e0 <*> e e1
    App e0 es            -> App <$> e e0 <*> traverse e es

instance Traversable8 Type where
  traverse8 i _ _ t _ _ _ _ = \case
    Void             -> pure Void
    NumType s nt     -> pure $ NumType s nt
    NominalType s i0 -> NominalType s <$> i i0
    Ptr t0           -> Ptr <$> t t0
    FunPtr t0 ts     -> FunPtr <$> t t0 <*> traverse t ts

instance Traversable8 TyIdents where
  traverse8 _ ti _ _ _ _ _ _  = tyIdents `traverseOf` traverse ti

instance Traversable8 TyIdent where
  traverse8 i _ _ t _ _ _ _ (TyIdent t0 i0) = TyIdent <$> t t0 <*> i i0

instance Traversable8 Ident where
  traverse8 _ _ _ _ _ _ _ _ = pure . coerce



instance NonTerminal Items where
  length = fromIntegral . S.length . _items
  canSelectRange _ = True
  canDescend _ = True
  indexC  (Items s) i _ _ _ _ _ _ k _ = k $ S.index s $ fromIntegral i
  modifyC (Items s) i _ _ _ _ _ _ k _ = Items <$> flip (S.update i') s <$> k (S.index s i')
    where i' = fromIntegral i
  deleteX fromFor = items %~ deleteSubSeq fromFor

instance NonTerminal Item where
  length = \case
    Nominal _ _ _ -> 2
    Function _ _ _ -> 3
  canSelectRange _ = False
  canDescend _ = True
  indexC  it ix  i _ tis _ _ b _ _ = case it of
    Nominal _ i0 tis0    -> case ix of
      0 -> i i0
      1 -> tis tis0
    Function  i0 tis0 blk -> case ix of
      0 -> i i0
      1 -> tis tis0
      2 -> b blk
  modifyC it ix  i _ tis _ _ b _ _ = case it of
    Nominal s i0 tis0     -> case ix of
      0 -> (\x -> Nominal s x tis0) <$> i i0
      1 -> (\x -> Nominal s i0 x) <$> tis tis0
    Function  i0 tis0 blk -> case ix of
      0 -> (\x -> Function x tis0 blk) <$> i i0
      1 -> (\x -> Function i0 x blk) <$> tis tis0
      2 -> (\x -> Function i0 tis0 x) <$> b blk
  deleteX _ = id

instance NonTerminal Block where
  length = fromIntegral . S.length . _block
  canSelectRange _ = True
  canDescend _ = True
  indexC  (Block s) i _ _ _ _ k _ _ _ = k $ S.index s $ fromIntegral i
  modifyC (Block s) i _ _ _ _ k _ _ _ = Block <$> flip (S.update i') s <$> k (S.index s i')
    where i' = fromIntegral i
  deleteX fromFor = block %~ deleteSubSeq fromFor

instance NonTerminal Expr where
  length = \case
    Ident _ -> 1
    Decl _ _ -> 2
    PrimMonOp _ _ -> 1
    PrimBinOp _ _ _ -> 2
    App _ s -> fromIntegral (S.length s) + 1
  canSelectRange = \case
    App _ _ -> True
    _ -> False
  canDescend _ = True
  indexC  it ix  i ti tis _ e b _ _ = case it of
    Ident txt -> case ix of
      0 -> i txt
    Decl ti0 e0 -> case ix of
      0 -> ti ti0
      1 -> e e0
    PrimMonOp _ e0 -> case ix of
      0 -> e e0
    PrimBinOp _ e0 e1 -> case ix of
      0 -> e e0
      1 -> e e1
    App e0 es -> case ix of
      0 -> e e0
      n -> e (es `S.index` fromIntegral (n - 1))
  modifyC  it ix  i ti tis _ e b _ _ = case it of
    Ident txt -> case ix of
      0 -> Ident <$> i txt
    Decl ti0 e0 -> case ix of
      0 -> (\x -> Decl x e0) <$> ti ti0
      1 -> (\x -> Decl ti0 x) <$> e e0
    PrimMonOp s e0 -> case ix of
      0 -> PrimMonOp s <$> e e0
    PrimBinOp s e0 e1 -> case ix of
      0 -> (\x -> PrimBinOp s x e1) <$> e e0
      1 -> (\x -> PrimBinOp s e0 x) <$> e e1
    App e0 es -> case ix of
      0 -> (\x -> App x es) <$> e e0
      n -> (\x -> App e0 x) <$> flip (S.update n') es <$> e (S.index es n')
        where n' = fromIntegral $ n - 1
  deleteX _ = id

instance NonTerminal Type where
  length = \case
    Void             -> 0
    NumType _ _      -> 0
    NominalType _ _  -> 1
    Ptr _            -> 1
    FunPtr _ ts      -> fromIntegral $ 1 + S.length ts
  canSelectRange = \case
    FunPtr _ _ -> True
    _          -> False
  canDescend = \case
    Void            -> False
    NumType _ _     -> False
    NominalType _ _ -> True
    Ptr _           -> True
    FunPtr _ _      -> True
  indexC  it ix  i ti tis t _ _ _ _ = case it of
    Void             -> undefined
    NumType s nt     -> undefined
    NominalType s i0 -> case ix of
      0 -> i i0
    Ptr t0           -> case ix of
      0 -> t t0
    FunPtr t0 ts     -> case ix of
      0 -> t t0
      n -> t (ts `S.index` fromIntegral (n - 1))
  modifyC it ix  i ti tis t _ _ _ _ = case it of
    Void             -> undefined
    NumType s nt     -> undefined
    NominalType s i0 -> case ix of
      0 -> NominalType s <$> i i0
    Ptr t0           -> case ix of
      0 -> Ptr  <$> t t0
    FunPtr t0 ts     -> case ix of
      0 -> (\x -> FunPtr x ts) <$> t t0
      n -> (\x -> FunPtr t0 x) <$> flip (S.update n') ts <$> t (S.index ts n')
        where n' = fromIntegral $ n - 1
  deleteX _ = id

instance NonTerminal TyIdents where
  length = fromIntegral . S.length . _tyIdents
  canSelectRange _ = True
  canDescend _ = True
  indexC  (TyIdents s) i _ k _ _ _ _ _ _ = k $ S.index s $ fromIntegral i
  modifyC (TyIdents s) i _ k _ _ _ _ _ _ = TyIdents <$> flip (S.update i') s <$> k (S.index s i')
    where i' = fromIntegral i
  deleteX fromFor = tyIdents %~ deleteSubSeq fromFor

instance NonTerminal TyIdent where
  length _ = 2
  canSelectRange _ = False
  canDescend _ = True
  indexC  (TyIdent t0 i0) ix  i _ _ t _ _ _ _ = case ix of
    0 -> t t0
    1 -> i i0
  modifyC (TyIdent t0 i0) ix  i _ _ t _ _ _ _ = case ix of
    0 -> (\x -> TyIdent x i0) <$> t t0
    1 -> (\x -> TyIdent t0 x) <$> i i0
  deleteX _ = id

instance NonTerminal Ident where
  length _ = 0
  canSelectRange _ = False
  canDescend _ = False
  indexC  _ _ _ _ _ _ _ _ _ _ = error "Can't index C ident"
  modifyC _ _ _ _ _ _ _ _ _ _ = error "Can't modify C ident"
  deleteX _ = id



instance Completable Items where
  identifier _ = Nothing
  introductions = M.singleton "" $ Items [()]

instance Completable Item where
  identifier _ = Nothing
  introductions = M.fromList
    [ ("struct", Nominal Struct () ())
    , ("union", Nominal Union () ())
    , ("function", Function () () ())
    ]

instance Completable Block where
  identifier _ = Nothing
  introductions = M.singleton "" $ Block [()]

instance Completable Expr where
  identifier _ = Nothing
  introductions = M.fromList
    [ ("identifier", Ident ())
    , ("variable", Decl () ())
    , ("apply", App () [()])

    , ("reference",   PrimMonOp Ref ())
    , ("&",           PrimMonOp Ref ())
    , ("dereference", PrimMonOp Deref ())
    , ("*",           PrimMonOp Deref ())

    , ("add",         PrimBinOp Add     () ())
    , ("+",           PrimBinOp Add     () ())
    , ("subtract",    PrimBinOp Sub     () ())
    , ("-",           PrimBinOp Sub     () ())
    , ("multiply",    PrimBinOp Mult    () ())
    , ("**",          PrimBinOp Mult    () ())
    , ("divide",      PrimBinOp Divide  () ())
    , ("/",           PrimBinOp Divide  () ())
    , ("logical or",  PrimBinOp BoolOr  () ())
    , ("||",          PrimBinOp BoolOr  () ())
    , ("logical and", PrimBinOp BoolAnd () ())
    , ("&&",          PrimBinOp BoolAnd () ())
    , ("bitwise or",  PrimBinOp BinOr   () ())
    , ("|",           PrimBinOp BinOr   () ())
    , ("bitwise and", PrimBinOp BinAnd  () ())
    , ("&",           PrimBinOp BinAnd  () ())
    , ("bitwise xor", PrimBinOp BinXor  () ())
    , ("^",           PrimBinOp BinXor  () ())
    ]

instance Completable Type where
  identifier _ = Nothing
  introductions = M.fromList
    [ ("void", Void)

    , ("signed char",  NumType Signed Char)
    , ("signed short", NumType Signed Short)
    , ("signed int",   NumType Signed Int)
    , ("signed long",  NumType Signed Long)

    , ("unsigned char",  NumType Signed Char)
    , ("unsigned short", NumType Signed Short)
    , ("unsigned int",   NumType Signed Int)
    , ("unsigned long",  NumType Signed Long)

    , ("struct", NominalType Struct ())
    , ("union", NominalType Struct ())

    , ("pointer", Ptr ())
    , ("*",       Ptr ())
    , ("function pointer", FunPtr () [()])
    ]

instance Completable TyIdent where
  identifier _ = Nothing
  introductions = M.singleton "" $ TyIdent () ()

instance Completable TyIdents where
  identifier _ = Nothing
  introductions = M.singleton "" $ TyIdents [()]

instance Completable Ident where
  identifier i = Just $ Text i
  introductions = M.empty


emptySelect = Select $ Range (0,0)

freshEmptySelect :: (Fresh ann, Monad m)
                 => FreshT ann m (ann, SelectionInner Word)
freshEmptySelect = (,emptySelect) <$> getFresh

freshAtom txt = (\ann -> ann :< Atom txt) <$> freshEmptySelect

pattern TIR ty ident <- (_ :< Node [ty, ident])

instance RenderableNonTerminal Items where
  convertNT = pure . LiftBf8 . Node . _items

nomSortAtom :: (Fresh ann, Monad m)
            => NominalSort
            -> FreshT ann m (Tree Text (ann, SelectionInner Word))
nomSortAtom = freshAtom . \case
  Struct -> "struct"
  Union  -> "union"

instance RenderableNonTerminal Item where
  convertNT = fmap LiftBf8 . \case
    Nominal sort name fields -> do
      tag <- nomSortAtom sort
      return $ Node [tag, name, fields]
    Function retTy args body -> return $ Node [retTy, args, body]

instance RenderableNonTerminal Block where
  convertNT = pure . LiftBf8 . Node . _block

instance RenderableNonTerminal Expr where
  convertNT = fmap LiftBf8 . \case
    Ident (_ :< ident) -> pure ident
    Decl tyIdent expr -> do
      eq <- freshAtom "="
      pure $ Node [tyIdent, eq, expr]
    App fun args -> do
      pure $ Node $ fun S.<| args
    PrimMonOp sort expr -> do
      tag <- freshAtom $ case sort of
        Ref     -> "&"
        Deref   -> "*"
        BoolInv -> "!"
        BinInv  -> "*"
      return $ Node [tag, expr]
    PrimBinOp sort e0 e1 ->  do
      tag <- freshAtom $ case sort of
        Add     -> "+"
        Sub     -> "-"
        Mult    -> "*"
        Divide  -> "/"
        BoolOr  -> "||"
        BoolAnd -> "&&"
        BinOr   -> "|"
        BinAnd  -> "&"
        BinXor  -> "^"
      return $ Node [e0, tag, e1]

instance RenderableNonTerminal Type where
  convertNT = fmap LiftBf8 . \case
    Void             -> return $ Atom "Void"
    NumType s nt     -> do
      s' <- freshAtom $ case s of
        Signed -> "signed"
        Unsigned -> "unsigned"
      nt' <- freshAtom $ case nt of
        Char  -> "char"
        Short -> "short"
        Int   -> "int"
        Long  -> "long"
      return $ Node [s', nt']

    NominalType s i0 -> do
      s' <- nomSortAtom s
      return $ Node [s', i0]
    Ptr t0           -> do
      star <- freshAtom "*"
      return $ Node [star, t0]
    FunPtr t0 ts     -> do
      star <- freshAtom "(*)"
      return $ Node $ t0 S.<| star S.<| ts


instance RenderableNonTerminal TyIdents where
  convertNT = pure . LiftBf8 . Node . _tyIdents

instance RenderableNonTerminal TyIdent where
  convertNT (TyIdent ty ident) = return $ LiftBf8 $ Node [ty, ident]

instance RenderableNonTerminal Ident where
  convertNT = pure . LiftBf8 . Atom . _text


instance Language (WithHole Ident)    (WithHole TyIdent)
                  (WithHole TyIdents) (WithHole Type)
                  (WithHole Expr)     (WithHole Block)
                  (WithHole Item)     (WithHole Items) where

  type Ann (WithHole Ident)    (WithHole TyIdent)
           (WithHole TyIdents) (WithHole Type)
           (WithHole Expr)     (WithHole Block)
           (WithHole Item)     (WithHole Items) = Ann'

  type Accum (WithHole Ident)    (WithHole TyIdent)
             (WithHole TyIdents) (WithHole Type)
             (WithHole Expr)     (WithHole Block)
             (WithHole Item)     (WithHole Items) = Accum'

  handleEvent = runStateOnly . handleEvent'

  getMessage a@(_, s) = ( "C: " <> printMode (_mode s)
                        , renderChoices a)

type AccumP = (Trunk, Accum')

type Term'' n = C n Ann'
type Trunk  = Term'' 7

initState :: AccumP
initState = ( (0, Select $ Single 0) `CF7` Unfilled
            , Editor
              { _mode = Normal
              , _currentId = 1
              })

mapCursorAnn :: (Word -> Word)
             -> (ann, Selection)
             -> (ann, Selection)
mapCursorAnn f = fmap $ \case
  Descend i             -> Descend $ f i
  Select (Single i)     -> Select $ Range (f i, f i + 1)
  Select (Range (a, b)) -> Select $ Range (f a, case b of
                                           0 -> f 0
                                           n -> f (b - 1) + 1)

rebuild :: (ann, Selection)
        -> Fundamental.CursorInner' (LiftBf8 Element Text) Text ann
        -> (Word -> Word)
        -> Fundamental.Cursor' Text ann
rebuild a r f = CF7 (mapCursorAnn f a) r

conv :: AccumP  -> Fundamental.Cursor' Text Word
conv (t, e) = fst $ runIdentity $ runFreshT (conv' t) $ e^.currentId

conv' :: (Monad m, Fresh ann)
      => C n ann -> FreshT ann m (Fundamental.Cursor' Text ann)
conv' = \case
  CF0 a r -> do
    inner <- convertNT =<< traverse8 conv' conv' conv' conv' conv' conv' conv' conv' r
    return $ rebuild a inner $ id
  CF1 a r -> do
    inner <- convertNT =<< traverse8 conv' conv' conv' conv' conv' conv' conv' conv' r
    return $ rebuild a inner $ id
  CF2 a r -> do
    inner <- convertNT =<< traverse8 conv' conv' conv' conv' conv' conv' conv' conv' r
    return $ rebuild a inner $ id
  CF3 a r -> do
    inner <- convertNT =<< traverse8 conv' conv' conv' conv' conv' conv' conv' conv' r
    return $ rebuild a inner $ case r of
      Unfilled -> id
      Filled other -> case other of
        Void            -> id
        NumType _ _     -> id
        NominalType _ _ -> (+1)
        Ptr _           -> id
        FunPtr _ _      -> id
  CF4 a r -> do
    inner <- convertNT =<< traverse8 conv' conv' conv' conv' conv' conv' conv' conv' r
    return $ rebuild a inner $  case r of
      Unfilled -> id
      Filled other -> case other of
        Ident _ -> id
        Decl _ _        -> \case
          0             -> 0
          1             -> 2
        PrimMonOp _ _   -> (+1)
        PrimBinOp _ _ _ -> \case
          0             -> 0
          1             -> 2
        App _ _         -> id
  CF5 a r -> do
    inner <- convertNT =<< traverse8 conv' conv' conv' conv' conv' conv' conv' conv' r
    return $ rebuild a inner $ id
  CF6 a r -> do
    inner <- convertNT =<< traverse8 conv' conv' conv' conv' conv' conv' conv' conv' r
    return $ rebuild a inner $ case r of
      Unfilled -> id
      Filled (Nominal _ _ _) -> (+1)
      Filled (Function _ _ _) -> id
  CF7 a r -> do
    inner <- convertNT =<< traverse8 conv' conv' conv' conv' conv' conv' conv' conv' r
    return $ rebuild a inner $ id

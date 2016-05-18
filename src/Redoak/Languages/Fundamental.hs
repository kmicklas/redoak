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

module Redoak.Languages.Fundamental where

import Control.Lens hiding ((:<))
import Control.Lens.TH
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Bifoldable
import Data.Bifunctor
import Data.Bifunctor.TH
import Data.Bitraversable
import Data.Coerce
import Data.Map (Map, fromList, empty)
import Data.Maybe
import Data.MonoTraversable hiding (Element)
import Data.Monoid
import Data.Sequence as S hiding ((:<))
import Data.Sequences as SS
import Data.Text as T hiding (copy)
import Data.Void

import Control.Comonad.Cofree8
import Data.Functor8
import Data.Foldable8
import Data.Traversable8

import Redoak.Event
import Redoak.Language
import Redoak.Language.DefaultInput
import Redoak.Languages.Empty


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
  Node s -> Node $ f s


instance IsSequence a => NonTerminal (LiftBf8 Element a) where
  length (LiftBf8 e) = fromIntegral $ elimIsSequence olength e

  canSelectRange _ = True

  canDescend (LiftBf8 e) = case e of
    Atom _ -> False
    Node _ -> True

  indexC (LiftBf8 e) i _ _ _ _ _ _ _ k = case e of
    Atom _ -> error "indexC: can't' descend into atom"
    Node s -> k $ S.index s (fromIntegral i)

  modifyC (LiftBf8 e) i _ _ _ _ _ _ _ k = case e of
    Atom _ -> error "modifyC: can't' descend into atom"
    Node s -> LiftBf8 <$> Node <$> flip (S.update i') s <$> k (S.index s i')
      where i' = fromIntegral i

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
       => EditT' (FreshT ann m) atom ann ()
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
       -> EditT' (FreshT ann m) atom ann ()
change new = local $ do
    (a, Select (Range (start, end))) `CF7` (LiftBf8 old) <- get

    let insert :: forall seq. IsSequence seq
               => seq
               -> seq
               -> (seq -> Trunk atom (ann, Selection))
               -> EditT' (FreshT ann m) atom ann ()
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
           => EditT' (FreshT ann m) a ann ()
insertNode = do
  id <- lift getFresh
  change (Node [(id, Select $ Range (0, 0)) :< Node []])

reverse :: (IsSequence a, Fresh ann, Monad m) => EditT' (FreshT ann m) a ann ()
reverse = local $ do
  _ `CF7` (LiftBf8 _) <- get -- desugared pattern match needed for absurd pattern
  getSelection >>= (change . mapIsSequence SS.reverse)

-- * Derived Edits

wrap :: (IsSequence a, Fresh ann, Monad m) => EditT' (FreshT ann m) a ann ()
wrap = do
  sel <- getSelection
  push
  change sel

unwrap :: (IsSequence a, Fresh ann, Monad m) => MaybeEditT' (FreshT ann m) a ann ()
unwrap = do
  sel <- getSelection
  ascend
  justEdit $ change sel

-- | Create new node, edit at begining of it
push :: (IsSequence a, Fresh ann, Monad m) => EditT' (FreshT ann m) a ann ()
push = insertNode >> assumeMaybeEdit descend


instance Language Void8 Void8 Void8 Void8 Void8 Void8 Void8 (LiftBf8 Element Text) where

  type Ann Void8 Void8 Void8 Void8 Void8 Void8 Void8 (LiftBf8 Element Text) = Word

  type Accum Void8 Void8 Void8 Void8 Void8 Void8 Void8 (LiftBf8 Element Text) = Editor

  handleEvent e = runStateOnly $ do
    onEvent e
    get >>= return . mode . snd >>= \case
      Insert -> onEventInsert e
      Normal -> onEventNormal e

  getMessage (_, s) = ( "Fundamental: " <> (T.pack $ show $ mode s)
                      , return ())

type Ann'   = Ann   Void8 Void8 Void8 Void8 Void8 Void8 Void8 (LiftBf8 Element Text)
type Accum' = Accum Void8 Void8 Void8 Void8 Void8 Void8 Void8 (LiftBf8 Element Text)
type Term'  = Term  Void8 Void8 Void8 Void8 Void8 Void8 Void8 (LiftBf8 Element Text) 7

type AccumP = (Term' , Accum')

data Editor
  = Editor
    { mode :: !Mode
    , currentId :: !Word
    , clipboard :: Trunk Text ()
    }

data Mode
  = Normal
  | Insert
  deriving (Eq, Ord, Show)

initState :: AccumP
initState = ( (0, Select $ Range (0, 0)) :< Node []
            , Editor
              { mode = Normal
              , currentId = 1
              , clipboard = Node []
              })
  where x = (0, Select $ Range (0, 0)) :< Node []

apply :: EditT' (FreshT Word Identity) Text Word a -> State AccumP a
apply e = do
  (c, s) <- get
  let Identity ((r, c'), id') = runFreshT (runStateT e $ c) $ currentId s
  put $ (c', s { currentId = id' })
  return r

inMode :: Mode -> State AccumP () -> State AccumP ()
inMode m a = do
  (_, s) <- get
  when (mode s == m) a

gotoMode :: Mode -> State AccumP ()
gotoMode m = modify $ second $ \ s -> s { mode = m }

copy :: State AccumP ()
copy = do
  sel <- fmap clearAnn <$> apply getSelection
  modify $ fmap $ \s -> s { clipboard = sel }

paste :: State AccumP ()
paste = do
  (_, s) <- get
  new <- apply $ lift $ mapM initAnn $ clipboard s
  apply $ change $ second initCursor $ new

deleteBackward :: (IsSequence a, Fresh ann, Monad m)
               => MaybeEditT' (FreshT ann m) a ann ()
deleteBackward = do
  e <- isEmpty
  if e
  then moveLeft >> justEdit delete
  else justEdit delete

deleteForward :: (IsSequence a, Fresh ann, Monad m)
              => MaybeEditT' (FreshT ann m) a ann ()
deleteForward = do
  e <- isEmpty
  if e
  then moveRight >> justEdit delete
  else justEdit delete

pushNode :: (IsSequence a, Fresh ann, Monad m)
            => MaybeEditT' (FreshT ann m) a ann ()
pushNode = do
  a <- isInAtom
  if a
  then pop >> justEdit push
  else justEdit push

insert :: (IsSequence a, Fresh ann, Monad m)
       => a -> MaybeEditT' (FreshT ann m) a ann ()
insert text = do
  justEdit (change $ Atom text)
  justEdit (tryEdit $ descend >> endMax)
  selectNoneEnd

onEvent :: KeyEvent -> State AccumP ()
onEvent e = (apply <$> basicTraversal e) `orElse` case e of
  KeyStroke Down Tab (Modifiers _ _ False) -> apply $ tryEdit pushNode

  KeyStroke Down Backspace _ -> apply $ tryEdit deleteBackward
  KeyStroke Down Delete    _ -> apply $ tryEdit deleteForward

  _ -> return ()

onEventNormal :: KeyEvent -> State AccumP ()
onEventNormal = \case

  KeyPress 'i' -> apply $ tryEdit ascend
  KeyPress 'k' -> apply $ tryEdit descend
  KeyPress 'j' -> apply $ tryEdit shiftLeft
  KeyPress 'l' -> apply $ tryEdit shiftRight
  KeyPress 'J' -> apply $ tryEdit moveLeft
  KeyPress 'L' -> apply $ tryEdit moveRight

  KeyPress 'a' -> apply $ tryEdit selectAll
  KeyPress 's' -> apply $ tryEdit switchBounds
  KeyPress 'd' -> apply delete
  KeyPress 'f' -> apply $ tryEdit selectNoneEnd
  KeyPress 'g' -> apply $ tryEdit selectOne

  KeyPress 'c' -> copy
  KeyPress 'x' -> copy >> apply delete
  KeyPress 'v' -> paste

  KeyPress 'n' -> apply insertNode
  KeyPress 'r' -> apply Redoak.Languages.Fundamental.reverse

  KeyPress 'w' -> apply wrap
  KeyPress 'e' -> apply $ tryEdit unwrap

  KeyPress 'h' -> gotoMode Insert

  _ -> return ()

onEventInsert :: KeyEvent -> State AccumP ()
onEventInsert = \case

  KeyStroke Down Enter (Modifiers _ _ False) -> gotoMode Normal
  KeyStroke Down Space (Modifiers _ _ False) -> apply $ tryEdit pop

  KeyStroke Down Enter (Modifiers _ _ True) -> apply $ tryEdit $ insert "\n"
  KeyStroke Down Space (Modifiers _ _ True) -> apply $ tryEdit $ insert " "
  KeyStroke Down Tab   (Modifiers _ _ True) -> apply $ tryEdit $ insert "\t"

  KeyPress c -> apply $ tryEdit $ insert [c]
  _ -> return ()

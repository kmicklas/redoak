{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Redoak.Language.Hole where

import qualified Prelude
import           Prelude hiding (length)

import           Control.Lens hiding (index, (:<))
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Constraint
import           Data.Functor.Identity
import           Data.Map as M
import           Data.Maybe
import           Data.Proxy
import           Data.Text as T hiding (length, index)
import           Data.Type.Equality hiding (apply)
import           GHC.TypeLits
import           Reflex.Dom (MonadWidget)

import           Control.Comonad.Cofree8
import           Data.Functor8
import           Data.Foldable8
import           Data.Traversable8

import           Redoak.Event
import           Redoak.Layout
import           Redoak.Rectangle
import           Redoak.Layout.Identity
import           Redoak.View
import           Redoak.Language hiding (index)
import           Redoak.Language.DefaultInput
import           Redoak.Languages.Fundamental (LiftBf8(..), Element(..))




instance Completable Void8 where
  identifiers _ = Nothing
  introductions = M.empty

class NonTerminal f => Completable f where
  identifiers :: Text -> Maybe (f () () () () () () () ())
  -- | Introduction rules for auto-complete
  introductions :: Map Text (f () () () () () () () ())

type CompletableAll f0 f1 f2 f3 f4 f5 f6 f7 =
  ( Completable f0, Completable f1, Completable f2, Completable f3
  , Completable f4, Completable f5, Completable f6, Completable f7)

data WithHole f a0 a1 a2 a3 a4 a5 a6 a7
  = Filled   (f a0 a1 a2 a3 a4 a5 a6 a7)
  | Unfilled
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Functor8 f => Functor8 (WithHole f) where
  map8 f0 f1 f2 f3 f4 f5 f6 f7 = \case
    Filled e -> Filled $ map8 f0 f1 f2 f3 f4 f5 f6 f7 e
    Unfilled -> Unfilled

instance Foldable8 f => Foldable8 (WithHole f) where
  foldMap8 f0 f1 f2 f3 f4 f5 f6 f7 = \case
    Filled e -> foldMap8 f0 f1 f2 f3 f4 f5 f6 f7 e
    Unfilled -> mempty

instance Traversable8 f => Traversable8 (WithHole f) where
  traverse8 f0 f1 f2 f3 f4 f5 f6 f7 = \case
    Filled e -> Filled <$> traverse8 f0 f1 f2 f3 f4 f5 f6 f7 e
    Unfilled -> pure Unfilled


instance NonTerminal f => NonTerminal (WithHole f) where
  length = \case
    (Filled e) -> length e
    Unfilled   -> 0

  canSelectRange = \case
    (Filled e) -> canSelectRange e
    Unfilled   -> False

  canDescend = \case
    (Filled e) -> canDescend e
    Unfilled   -> False

  indexC e i = case e of
    (Filled e) -> indexC e i
    Unfilled   -> \ _ _ _ _ _ _ _ _ -> undefined

  modifyC e i f0 f1 f2 f3 f4 f5 f6 f7 = case e of
    (Filled e) -> Filled <$> modifyC e i f0 f1 f2 f3 f4 f5 f6 f7
    Unfilled   -> undefined -- could define with Applicative and pure,
                            -- but no valid index in this case anyways

--instance Completable f => Completable (WithHole f) where
--  introductions = Filled <$> introductions


type CursorWithHole f0 f1 f2 f3 f4 f5 f6 f7  n ann =
  Cofree8' (WithHole f0) (WithHole f1) (WithHole f2) (WithHole f3)
           (WithHole f4) (WithHole f5) (WithHole f6) (WithHole f7)
           n (ann, Selection)
type CursorInnerWithHole f f0 f1 f2 f3 f4 f5 f6 f7 ann =
  CursorInner (WithHole f)
              (WithHole f0) (WithHole f1) (WithHole f2) (WithHole f3)
              (WithHole f4) (WithHole f5) (WithHole f6) (WithHole f7)
              ann

type EditWithHoleT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r =
  StateT (CursorWithHole f0 f1 f2 f3 f4 f5 f6 f7  n ann) m r
type EditWithHole f0 f1 f2 f3 f4 f5 f6 f7  n ann r =
  EditWithHoleT Identity f0 f1 f2 f3 f4 f5 f6 f7  n ann r

type MaybeEditWithHoleT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r =
  EditWithHoleT (MaybeT m) f0 f1 f2 f3 f4 f5 f6 f7  n ann r

-- Eww
makeHole :: forall m n ann  f0 f1 f2 f3 f4 f5 f6 f7
         .  ( CompletableAll f0 f1 f2 f3 f4 f5 f6 f7
            , Monad m, Fresh ann, KnownNat n)
         => FreshT ann m (CursorWithHole f0 f1 f2 f3 f4 f5 f6 f7  n ann)
makeHole = case sameNat (Proxy :: Proxy n) (Proxy :: Proxy 0) of
  Just prf  -> gcastWith prf $ CF0 <$> ann <*> pure Unfilled
  Nothing -> case sameNat (Proxy :: Proxy n) (Proxy :: Proxy 1) of
    Just prf  -> gcastWith prf $ CF1 <$> ann <*> pure Unfilled
    Nothing -> case sameNat (Proxy :: Proxy n) (Proxy :: Proxy 2) of
      Just prf  -> gcastWith prf $ CF2 <$> ann <*> pure Unfilled
      Nothing -> case sameNat (Proxy :: Proxy n) (Proxy :: Proxy 3) of
        Just prf  -> gcastWith prf $ CF3 <$> ann <*> pure Unfilled
        Nothing -> case sameNat (Proxy :: Proxy n) (Proxy :: Proxy 4) of
          Just prf  -> gcastWith prf $ CF4 <$> ann <*> pure Unfilled
          Nothing -> case sameNat (Proxy :: Proxy n) (Proxy :: Proxy 5) of
            Just prf  -> gcastWith prf $ CF5 <$> ann <*> pure Unfilled
            Nothing ->case sameNat (Proxy :: Proxy n) (Proxy :: Proxy 6) of
              Just prf  -> gcastWith prf $ CF6 <$> ann <*> pure Unfilled
              Nothing ->case sameNat (Proxy :: Proxy n) (Proxy :: Proxy 7) of
                Just prf  -> gcastWith prf $ CF7 <$> ann <*> pure Unfilled
                Nothing -> undefined
  where
     ann :: FreshT ann m (ann, Selection)
     ann = (, Select $ Single 0) <$> getFresh

prunePrefix :: Text -> Map Text a -> Map Text a
prunePrefix prefix = M.filterWithKey $ \ k _ -> isJust $ stripPrefix prefix k

makeChoices :: forall m n ann r  f0 f1 f2 f3 f4 f5 f6 f7
                          dum    d0 d1 d2 d3 d4 d5 d6 d7
            .  (CompletableAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m, Fresh ann)
            => Text
            -> CursorWithHole d0 d1 d2 d3 d4 d5 d6 d7 n dum
            -> FreshT ann m [CursorWithHole f0 f1 f2 f3 f4 f5 f6 f7 n ann]
makeChoices prefix = \case
    CF0 a r -> (fmap . fmap) (uncurry CF0) mkPair
    CF1 a r -> (fmap . fmap) (uncurry CF1) mkPair
    CF2 a r -> (fmap . fmap) (uncurry CF2) mkPair
    CF3 a r -> (fmap . fmap) (uncurry CF3) mkPair
    CF4 a r -> (fmap . fmap) (uncurry CF4) mkPair
    CF5 a r -> (fmap . fmap) (uncurry CF5) mkPair
    CF6 a r -> (fmap . fmap) (uncurry CF6) mkPair
    CF7 a r -> (fmap . fmap) (uncurry CF7) mkPair
  where
    mkPair  :: forall f
            .  Completable f
            => FreshT ann m [( (ann, Selection)
                             , CursorInnerWithHole f f0 f1 f2 f3 f4 f5 f6 f7  ann)]
    mkPair = do
      ts <- mkInners
      forM ts $ \t -> do
        a <- getFresh
        let sel = Select $ if canSelectRange t
              then Range (0, 0)
              else Single 0
        return ((a, sel), t)

    mkInners :: forall f
             .  Completable f
             => FreshT ann m [CursorInnerWithHole f f0 f1 f2 f3 f4 f5 f6 f7  ann]
    mkInners = sequence
              $ mapM8 (\_ -> makeHole) (\_ -> makeHole) (\_ -> makeHole) (\_ -> makeHole)
                      (\_ -> makeHole) (\_ -> makeHole) (\_ -> makeHole) (\_ -> makeHole)
              <$> Filled <$> choices prefix

fill :: forall m n ann r  f0 f1 f2 f3 f4 f5 f6 f7
     .  (CompletableAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m, Fresh ann)
     => Text
     -> Word
     -> MaybeEditWithHoleT (FreshT ann m) f0 f1 f2 f3 f4 f5 f6 f7 n ann ()
fill prefix index = local' $ modifyT $ lift . fmap (!! fromIntegral index) . makeChoices prefix

choices :: Completable f => Text -> [f () () () () () () () ()]
choices prefix = maybeToList (identifiers prefix) ++ M.elems (prunePrefix prefix introductions)

countChoices :: forall m n ann r  f0 f1 f2 f3 f4 f5 f6 f7
             .  (CompletableAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m, Fresh ann)
             => Text
             -> MaybeEditWithHoleT (FreshT ann m) f0 f1 f2 f3 f4 f5 f6 f7 n ann Word
countChoices prefix = local' $ do
  subtree <- get
  let count :: Int
      count = case subtree of
        CF0 a r -> Prelude.length (choices prefix :: [f0 () () () () () () () ()])
        CF1 a r -> Prelude.length (choices prefix :: [f1 () () () () () () () ()])
        CF2 a r -> Prelude.length (choices prefix :: [f2 () () () () () () () ()])
        CF3 a r -> Prelude.length (choices prefix :: [f3 () () () () () () () ()])
        CF4 a r -> Prelude.length (choices prefix :: [f4 () () () () () () () ()])
        CF5 a r -> Prelude.length (choices prefix :: [f5 () () () () () () () ()])
        CF6 a r -> Prelude.length (choices prefix :: [f6 () () () () () () () ()])
        CF7 a r -> Prelude.length (choices prefix :: [f7 () () () () () () () ()])
  return $ fromIntegral count

unfill :: (CompletableAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m, Fresh ann)
       => MaybeEditWithHoleT (FreshT ann m) f0 f1 f2 f3 f4 f5 f6 f7 n ann ()
unfill = do
  i <- guardSingle
  mapStatePoly ntfCls $ modifyStateC i $ put =<< lift (lift makeHole)


type Ann' = Word
type Accum' = Editor
type Term' f0 f1 f2 f3 f4 f5 f6 f7  n = CursorWithHole f0 f1 f2 f3 f4 f5 f6 f7
                                                       n Ann'
type Trunk' f0 f1 f2 f3 f4 f5 f6 f7 = Term' f0 f1 f2 f3 f4 f5 f6 f7  7

type AccumP' f0 f1 f2 f3 f4 f5 f6 f7 =
  (Trunk' f0 f1 f2 f3 f4 f5 f6 f7, Editor)

data Editor --f0 f1 f2 f3 f4 f5 f6 f7
  = Editor
    { _mode :: !Mode
    , _currentId :: !Word
--    , _clipboard :: Trunk' f0 f1 f2 f3 f4 f5 f6 f7
    }

data Mode
  = Normal
  | Filling
    { _index :: Word
    , _prefix :: Text
    }
  deriving (Eq, Ord, Show)

makeLenses ''Editor
makeLenses ''Mode

apply :: CompletableAll f0 f1 f2 f3 f4 f5 f6 f7
      => EditWithHoleT (FreshT Word Identity)  f0 f1 f2 f3 f4 f5 f6 f7  7 Word a
      -> State (AccumP' f0 f1 f2 f3 f4 f5 f6 f7) a
apply e = do
  (c, s) <- get
  let Identity ((r, c'), id') = runFreshT (runStateT e $ c) $ _currentId s
  put $ (c', (currentId .~ id') s)
  return r

gotoMode :: CompletableAll f0 f1 f2 f3 f4 f5 f6 f7
         => Mode -> State (AccumP' f0 f1 f2 f3 f4 f5 f6 f7) ()
gotoMode m = modify $ second $ mode .~ m

printMode :: Mode -> Text
printMode = \case
  Normal      -> "Normal"
  Filling _ _ -> "Filling"

handleEvent' :: CompletableAll f0 f1 f2 f3 f4 f5 f6 f7
             => KeyEvent -> State (AccumP' f0 f1 f2 f3 f4 f5 f6 f7) ()
handleEvent' e = get >>= return . _mode . snd >>= \case
  Normal      -> onEventNormal e
  Filling _ _ -> onEventFilling e

onEventNormal :: CompletableAll f0 f1 f2 f3 f4 f5 f6 f7
              => KeyEvent -> State (AccumP' f0 f1 f2 f3 f4 f5 f6 f7) ()
onEventNormal e = (apply <$> basicTraversal e) `orElse` case e of

  KeyPress 'i' -> apply $ tryEdit ascend
  KeyPress 'k' -> apply $ tryEdit descend
  KeyPress 'j' -> apply $ tryEdit shiftLeft
  KeyPress 'l' -> apply $ tryEdit shiftRight
  KeyPress 'J' -> apply $ tryEdit moveLeft
  KeyPress 'L' -> apply $ tryEdit moveRight

  KeyPress 'a' -> apply $ tryEdit selectAll
  KeyPress 's' -> apply $ tryEdit switchBounds
  --KeyPress 'd' -> apply delete
  KeyPress 'f' -> apply $ tryEdit selectNoneEnd
  KeyPress 'g' -> apply $ tryEdit selectOne


  KeyPress 'h' -> do
    single <- apply $ maybeEdit' $ local' guardSingle
    when (isJust single) $ gotoMode $ Filling 0 ""

  _ -> return ()

safeMinus = \case
  0 -> 0
  n -> n - 1

onEventFilling :: forall f0 f1 f2 f3 f4 f5 f6 f7
               .  CompletableAll f0 f1 f2 f3 f4 f5 f6 f7
               => KeyEvent -> State (AccumP' f0 f1 f2 f3 f4 f5 f6 f7) ()
onEventFilling = \case

  KeyStroke Down ArrowUp    (Modifiers _     _ _)      -> _2 . mode . index %= safeMinus
  KeyStroke Down ArrowDown  (Modifiers _     _ _)      -> do
    pf <- use $ _2 . mode . prefix
    mlen <- apply $ maybeEdit' $ countChoices pf
    case mlen of
      Nothing  -> return ()
      Just len -> _2 . mode . index %= (\i -> min (i + 1) len)

  KeyStroke Down Enter (Modifiers _ _ False) -> do
    pf <- use $ _2 . mode . prefix
    md <- use $ _2 . mode -- lens wants monoid cause partial
    status <- apply $ maybeEdit' $ fill pf (_index md)
    when (isJust status) $ gotoMode Normal

  KeyStroke Down Backspace _ -> do
    pf <- use $ _2 . mode . prefix
    if T.null pf
      then gotoMode Normal
      else _2 . mode . prefix %= T.init

  KeyStroke Down Space (Modifiers _ _ False) -> apply $ tryEdit pop

  KeyPress c -> _2 . mode . prefix %= (`T.snoc` c)
  _ -> return ()


renderChoices :: forall t m f0 f1 f2 f3 f4 f5 f6 f7 n a
                            d0 d1 d2 d3 d4 d5 d6 d7
              .  ( CompletableAll f0 f1 f2 f3 f4 f5 f6 f7, MonadWidget t m
                 , RenderableNonTerminal (WithHole f0) (LiftBf8 Element Text)
                 , RenderableNonTerminal (WithHole f1) (LiftBf8 Element Text)
                 , RenderableNonTerminal (WithHole f2) (LiftBf8 Element Text)
                 , RenderableNonTerminal (WithHole f3) (LiftBf8 Element Text)
                 , RenderableNonTerminal (WithHole f4) (LiftBf8 Element Text)
                 , RenderableNonTerminal (WithHole f5) (LiftBf8 Element Text)
                 , RenderableNonTerminal (WithHole f6) (LiftBf8 Element Text)
                 , RenderableNonTerminal (WithHole f7) (LiftBf8 Element Text))
              => AccumP' f0 f1 f2 f3 f4 f5 f6 f7
              -> CursorWithHole d0 d1 d2 d3 d4 d5 d6 d7 n a
              -> m ()
renderChoices s proxy = case s^._2.mode of
  Normal -> return ()
  Filling index prefix -> do
    let choiceList :: [CursorWithHole f0 f1 f2 f3 f4 f5 f6 f7 n Word]
        Identity (choiceList, id') = flip runFreshT (s^._2.currentId)
                                     $ makeChoices prefix proxy
        fundamentals :: [Cursor Void8 Void8 Void8 Void8
                                Void8 Void8 Void8 (LiftBf8 Element Text)
                                7 Word]
        fundamentals = upCast <$> choiceList where
    sequence_ $ makeNode . runIdentity . layout (W maxBound) <$> fundamentals

upCast :: forall f0 f1 f2 f3 f4 f5 f6 f7 n
       .  ( CompletableAll f0 f1 f2 f3 f4 f5 f6 f7
          , RenderableNonTerminal (WithHole f0) (LiftBf8 Element Text)
          , RenderableNonTerminal (WithHole f1) (LiftBf8 Element Text)
          , RenderableNonTerminal (WithHole f2) (LiftBf8 Element Text)
          , RenderableNonTerminal (WithHole f3) (LiftBf8 Element Text)
          , RenderableNonTerminal (WithHole f4) (LiftBf8 Element Text)
          , RenderableNonTerminal (WithHole f5) (LiftBf8 Element Text)
          , RenderableNonTerminal (WithHole f6) (LiftBf8 Element Text)
          , RenderableNonTerminal (WithHole f7) (LiftBf8 Element Text))
       => CursorWithHole f0 f1 f2 f3 f4 f5 f6 f7 n Word
       -> Cursor Void8 Void8 Void8 Void8 Void8 Void8 Void8
                 (LiftBf8 Element Text)
                 7 Word
upCast = \case
  CF0 a r -> a `CF7` convertNT (map8 upCast upCast upCast upCast upCast upCast upCast upCast r)
  CF1 a r -> a `CF7` convertNT (map8 upCast upCast upCast upCast upCast upCast upCast upCast r)
  CF2 a r -> a `CF7` convertNT (map8 upCast upCast upCast upCast upCast upCast upCast upCast r)
  CF3 a r -> a `CF7` convertNT (map8 upCast upCast upCast upCast upCast upCast upCast upCast r)
  CF4 a r -> a `CF7` convertNT (map8 upCast upCast upCast upCast upCast upCast upCast upCast r)
  CF5 a r -> a `CF7` convertNT (map8 upCast upCast upCast upCast upCast upCast upCast upCast r)
  CF6 a r -> a `CF7` convertNT (map8 upCast upCast upCast upCast upCast upCast upCast upCast r)
  CF7 a r -> a `CF7` convertNT (map8 upCast upCast upCast upCast upCast upCast upCast upCast r)

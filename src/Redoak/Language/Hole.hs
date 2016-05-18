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

import Prelude hiding (length)

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.Constraint
import Data.Functor.Identity
import Data.Map as M
import Data.Maybe
import Data.Proxy
import Data.Text as T hiding (length)
import Data.Type.Equality
import GHC.TypeLits

import Control.Comonad.Cofree8
import Data.Functor8
import Data.Foldable8
import Data.Traversable8

import Redoak.Language.Base
import Redoak.Language.Cursor
import Redoak.Language.Fresh


class NonTerminal f => Completable f where
  identifiers :: Text -> [(f () () () () () () () ())]
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

type EditMaybeWithHoleT m  f0 f1 f2 f3 f4 f5 f6 f7  n ann r =
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

fill :: forall m n ann r  f0 f1 f2 f3 f4 f5 f6 f7
     .  (CompletableAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m, Fresh ann)
     => Text
     -> Word
     -> EditWithHoleT (FreshT ann m) f0 f1 f2 f3 f4 f5 f6 f7 n ann ()
fill prefix index = modifyT $ \case
    CF0 a r -> CF0 a <$> go
    CF1 a r -> CF1 a <$> go
    CF2 a r -> CF2 a <$> go
    CF3 a r -> CF3 a <$> go
    CF4 a r -> CF4 a <$> go
    CF5 a r -> CF5 a <$> go
    CF6 a r -> CF6 a <$> go
    CF7 a r -> CF7 a <$> go
  where
    go :: forall f
       .  Completable f
       => FreshT ann m (CursorInnerWithHole f f0 f1 f2 f3 f4 f5 f6 f7  ann)
    go = mapM8 (\_ -> makeHole) (\_ -> makeHole) (\_ -> makeHole) (\_ -> makeHole)
               (\_ -> makeHole) (\_ -> makeHole) (\_ -> makeHole) (\_ -> makeHole)
               choice
      where
        choice = Filled $ choices !! fromIntegral index
        choices = identifiers prefix ++ M.elems (prunePrefix prefix introductions)

unfill :: (CompletableAll f0 f1 f2 f3 f4 f5 f6 f7, Monad m, Fresh ann)
       => EditMaybeWithHoleT (FreshT ann m) f0 f1 f2 f3 f4 f5 f6 f7 n ann ()
unfill = do
  i <- guardSingle
  mapStatePoly ntfCls $ modifyStateC i $ put =<< lift (lift makeHole)

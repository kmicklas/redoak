{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}

module Redoak
  ( editor
  ) where

import           Control.Monad.Identity
import           Data.List
import           Data.List.NonEmpty
import           Data.Text (Text)
import qualified Data.Text as T
import           GHCJS.DOM.Document (Document)
import           Reflex
import           Reflex.Dom

import           Redoak.Editor
import           Redoak.Event
import           Redoak.Layout
import           Redoak.Rectangle
import           Redoak.Tree
import           Redoak.Tree.Range
import           Redoak.View


divId :: MonadWidget t m => String -> m a -> m a
divId id = elAttr "div" ("id" =: id)

spanId :: MonadWidget t m => String -> m a -> m a
spanId id = elAttr "span" ("id" =: id)

defaultLayout :: Text -> [Text] -> Element Text View -> View
defaultLayout id classes =
  (ViewInfo (Just id) classes (W 0, H 0) (X 0, Y 0) :<)

editor :: MonadWidget t m => Event t (NonEmpty KeyEvent) -> m ()
editor keys = do
  stateStream <- foldDyn (flip $ foldl (flip handleEvent)) initState keys
  divId "editor" $ do
    divId "content" $ do
      _ <- dyn =<< mapDyn (makeNode . runIdentity . layout . cursor) stateStream
      return ()
    divId "status" $ do
      let pathString (is, (start, end)) =
            Data.List.intercalate ", " $ (fmap show is) ++
            [show start ++ if start == end then "" else "-" ++ show end]
      spanId "path" $ dynText =<< mapDyn (pathString . path . cursor) stateStream
      spanId "mode" $ dynText =<< mapDyn (show . mode) stateStream

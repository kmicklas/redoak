{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}

module Redoak.UI
  ( runEditor
  ) where

import           Control.Monad.Identity
import           Data.List
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


viewState :: MonadWidget t m => Editor -> m ()
viewState s = divId "editor" $ do
  divId "content" $
    makeNode $ runIdentity $ layout $ cursor s
  divId "status" $ do
    let pathString (is, (start, end)) =
          Data.List.intercalate ", " $ (fmap show is) ++
          [show start ++ if start == end then "" else "-" ++ show end]
    spanId "path" $ T.pack $ pathString $ path $ cursor s
    spanId "mode" $ T.pack $ show $ mode s

divId :: MonadWidget t m => String -> m a -> m a
divId id = elAttr "div" ("id" =: id)

spanId :: MonadWidget t m => String -> Text -> m ()
spanId id = elAttr "span" ("id" =: id) . text . T.unpack


defaultLayout :: Text -> [Text] -> Element Text View -> View
defaultLayout id classes =
  (ViewInfo (Just id) classes (W 0, H 0) (X 0, Y 0) :<)

runEditor :: MonadWidget t m => Document -> m ()
runEditor doc = do
  keys <- globalKeyEvents doc
  stateStream <- foldDyn (flip $ foldl (flip handleEvent)) initState keys
  _ <- dyn =<< mapDyn viewState stateStream
  return ()

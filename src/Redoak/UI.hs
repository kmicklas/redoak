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
import           Redoak.View


viewState :: Editor -> View
viewState s = defaultLayout "editor" [] $ Node [contentView, statusView]
  where contentView = defaultLayout "content" [] $ Node [treeView]
        treeView = runIdentity $ layout $ cursor s
        statusView = defaultLayout "status" [] $ Node [pathView, modeView]
        pathView = defaultLayout "path" [] $ Atom $ T.pack $ pathString $ path $ cursor s
        modeView = defaultLayout "mode" [] $ Atom $ T.pack $ show $ mode s
        pathString (is, (start, end)) =
          Data.List.intercalate ", " $ (fmap show is) ++
          [show start ++ if start == end then "" else "-" ++ show end]

defaultLayout :: Text -> [Text] -> Element Text View -> View
defaultLayout id classes =
  (ViewInfo (Just id) classes (W 0, H 0) (X 0, Y 0) :<)

runEditor :: MonadWidget t m => Document -> m ()
runEditor doc = do
  keys <- globalKeyEvents doc
  stateStream <- foldDyn (flip $ foldl (flip handleEvent)) initState keys
  _ <- dyn =<< mapDyn (makeNode . viewState) stateStream
  return ()

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}

module UI
  ( runEditor
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Sequence
import Data.Text
import GHCJS.DOM.Document (Document, keyDown, keyPress)
import GHCJS.DOM.EventM (on, preventDefault, stopPropagation, uiKeyCode, uiCharCode)

import Dom
import Editor
import Event
import Rectangle
import Tree
import View

viewState :: State -> View
viewState s = node "editor" [] [contentView, statusView]
  where contentView = node "content" [] [treeView]
        treeView = viewTree ["content"] $ tree $ cursor s
        statusView = node "status" [] [pathView, modeView]
        pathView = atom "path" [] $ pack $ pathString $ selection $ cursor s
        modeView = atom "mode" [] $ pack $ show $ mode s
        pathString (Path is (start, end)) =
          Data.List.intercalate ", " $ (fmap show is) ++
          [show start ++ if start == end then "" else "-" ++ show end]

atom :: Text -> [Text] -> Text -> View
atom id classes text =
  ViewInfo id classes (Width 0, Height 0) (X 0, Y 0) := Atom text

node :: Text -> [Text] -> Seq View -> View
node id classes children =
  ViewInfo id classes (Width 0, Height 0) (X 0, Y 0) := Node children

viewTree :: (Show i) => [Text] -> Tree Text i -> View
viewTree classes = \case
  id := Atom a  -> atom (pack $ show id) classes a
  id := Node ts -> node (pack $ show id) classes $ fmap (viewTree classes) ts

runEditor :: WithDoc ()
runEditor = do
  events <- newEmptyMVar
  on ?doc keyDown $ do
    getKey <$> uiKeyCode >>= \case
      Nothing -> return ()
      Just k  -> do
        stopPropagation
        preventDefault
        liftIO $ putMVar events $ KeyDown k
  on ?doc keyPress $ do
    liftIO . putMVar events . KeyPress . toEnum =<< uiCharCode
  forkIO $ react events onEvent (effectView . viewState) initState
  return ()

react :: MVar e -> (e -> s -> s) -> (s -> IO ()) -> s -> IO a
react events update effect = loop
  where loop state = do
          effect state
          event <- takeMVar events
          loop $ update event state

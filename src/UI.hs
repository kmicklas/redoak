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
import Control.Monad.Identity
import Data.Bifunctor
import Data.List
import Data.Sequence hiding ((:<))
import Data.Sequences as SS
import Data.Text
import GHCJS.DOM (WebView)
import GHCJS.DOM.Document (Document, keyDown, keyPress, getBody)
import GHCJS.DOM.EventM (on, event, preventDefault, stopPropagation, uiKeyCode, uiCharCode)
import GHCJS.DOM.KeyboardEvent (getCtrlKey, getAltKey, getShiftKey)
import Reflex.Dom

import Editor
import Event
import Layout
import Rectangle
import Tree
import View

viewState :: Editor -> View
viewState s = defaultLayout "editor" [] $ Node [contentView, statusView]
  where contentView = defaultLayout "content" [] $ Node [treeView]
        treeView = runIdentity $ layout $ cursor s
        statusView = defaultLayout "status" [] $ Node [pathView, modeView]
        pathView = defaultLayout "path" [] $ Atom $ pack $ pathString $ path $ cursor s
        modeView = defaultLayout "mode" [] $ Atom $ pack $ show $ mode s
        pathString (is, (start, end)) =
          Data.List.intercalate ", " $ (fmap show is) ++
          [show start ++ if start == end then "" else "-" ++ show end]

defaultLayout :: Text -> [Text] -> Element Text View -> View
defaultLayout id classes =
  (ViewInfo (Just id) classes (W 0, H 0) (X 0, Y 0) :<)

runEditor :: WebView -> Document -> IO ()
runEditor webView doc = do
  events <- newEmptyMVar
  on doc keyDown $ do
    getKey <$> uiKeyCode >>= \case
      Nothing -> return ()
      Just k  -> do
        stopPropagation
        preventDefault
        e <- event
        mod <- Modifiers <$> getCtrlKey e <*> getAltKey e <*> getShiftKey e
        liftIO $ putMVar events $ KeyDown k mod
  on doc keyPress $ do
    liftIO . putMVar events . KeyPress . toEnum =<< uiCharCode
  forkIO $ react events handleEvent (effectView webView doc . viewState) initState
  return ()

react :: MVar e -> (e -> s -> s) -> (s -> IO ()) -> s -> IO a
react events update effect = loop
  where loop state = do
          effect state
          event <- takeMVar events
          loop $ update event state

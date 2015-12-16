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
import Control.Monad.Identity
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.List
import Data.Sequence hiding ((:<))
import Data.Sequences as SS
import Data.Text
import GHCJS.DOM.Document (Document, keyDown, keyPress)
import GHCJS.DOM.EventM (on, preventDefault, stopPropagation, uiKeyCode, uiCharCode)

import Dom
import Editor
import Event
import Layout
import Rectangle
import Tree
import View

viewState :: Editor -> View
viewState s = defaultLayout "editor" [] $ Node [contentView, statusView, debugView]
  where contentView = defaultLayout "content" [] $ Node [treeView]
        treeView = runIdentity $ layout $ cursor s
        statusView = defaultLayout "status" [] $ Node [pathView, modeView]
        pathView = defaultLayout "path" [] $ Atom $ pack $ pathString $ path $ cursor s
        modeView = defaultLayout "mode" [] $ Atom $ pack $ show $ mode s
        debugView = defaultLayout "debug" [] $ Atom $ pack $ show s
        pathString (is, (start, end)) =
          Data.List.intercalate ", " $ (fmap show is) ++
          [show start ++ if start == end then "" else "-" ++ show end]

defaultLayout :: Text -> [Text] -> Element Text View -> View
defaultLayout id classes =
  (ViewInfo (Just id) classes (W 0, H 0) (X 0, Y 0) :<)

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
  forkIO $ react events handleEvent (effectView . viewState) initState
  return ()

react :: MVar e -> (e -> s -> s) -> (s -> IO ()) -> s -> IO a
react events update effect = loop
  where loop state = do
          effect state
          event <- takeMVar events
          loop $ update event state

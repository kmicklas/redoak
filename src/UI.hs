{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Tree
import View

viewState :: State -> View
viewState s = Node ("editor", []) $ fromList [contentView, statusView]
  where contentView = Node ("content", []) treeViews
        treeViews = viewTree ["content"] $ stringify $ tree $ cursor s
        statusView = Node ("status", []) $ fromList [pathView, modeView]
        pathView = Atom ("path", []) $ pack $ pathString $ selection $ cursor s
        modeView = Atom ("mode", []) $ pack $ show $ mode s
        pathString (Path is (start, end)) =
          Data.List.intercalate ", " $ (fmap show is) ++
          [show start ++ if start == end then "" else "-" ++ show end]

viewTree :: forall i. (Show i) => [Text] -> Tree i Text -> Seq View
viewTree classes = outer
  where
    outer = fmap viewElement

    viewElement :: Element i Text -> View
    viewElement = \case
      Atom i a  -> Atom (pack $ show i, classes) a
      Node i ts -> Node (pack $ show i, classes) $ outer ts

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

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
import Data.Bifunctor
import Data.List
import Data.Sequence
import Data.Sequences as SS
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
viewState s = defaultLayout "editor" [] $ Node [contentView, statusView]
  where contentView = defaultLayout "content" [] $ Node [treeView]
        treeView = viewCursor $ cursor s
        statusView = defaultLayout "status" [] $ Node [pathView, modeView]
        pathView = defaultLayout "path" [] $ Atom $ pack $ pathString $ path $ cursor s
        modeView = defaultLayout "mode" [] $ Atom $ pack $ show $ mode s
        pathString (is, (start, end)) =
          Data.List.intercalate ", " $ (fmap show is) ++
          [show start ++ if start == end then "" else "-" ++ show end]

-- | View a tree containing the selection
viewCursor :: (Show i) => Cursor Text i -> View
viewCursor (T ((id, sel) := e)) =
  defaultLayout (pack $ show id) ["content"] $ case (e, sel) of
    (Atom a, Select r) ->  makeSelection a r $ Atom
    (Node ts, Select r) -> makeSelection ts r $ Node . fmap viewTree
    (Node ts, Descend i) -> Node $ mconcat $
      [ fmap viewTree lPart
      , fmap viewCursor selPart
      , fmap viewTree rPart]
      where (lPart, selPart, rPart) = split ts $ (i, i + 1)

  where makeSelection :: IsSequence s
                      => s -> Range
                      -> (s -> Element Text View)
                      -> Element Text View
        makeSelection s r view = Node
          [ defaultLayout "left-of-selection"   [] $ view lPart
          , defaultLayout "selection" [dirClass r] $ view selPart
          , defaultLayout "right-of-selection"  [] $ view rPart
          ] where (lPart, selPart, rPart) = split s r

        split :: IsSequence s => s -> Range -> (s, s, s)
        split s (start, end) =
          ( SS.take (fromIntegral front) s
          , SS.take (fromIntegral $ back - front) $
              SS.drop (fromIntegral front) s
          , SS.drop (fromIntegral back) s
          ) where front = min start end
                  back  = max start end

        dirClass (start, end) =
          case compare start end of
            LT -> "cursor-at-back"
            GT -> "cursor-at-front"
            EQ -> "empty-selection"

-- | View a tree which doesn't contain the selection
viewTree :: (Show i) => Cursor Text i -> View
viewTree (T ((id, _) := e)) =
  defaultLayout (pack $ show id) classes $ case e of
    Atom a  -> Atom a
    Node ts -> Node $ fmap viewTree ts

  where classes = ["content"]

defaultLayout :: Text -> [Text] -> Element Text View -> View
defaultLayout id classes = T . (ViewInfo id classes (Width 0, Height 0) (X 0, Y 0) :=)

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

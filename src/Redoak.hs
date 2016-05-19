{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Redoak
  ( editor
  ) where

import           Control.Monad.Identity
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.List
import           Data.List.NonEmpty
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Foldable
import           GHCJS.DOM.Document (Document)
import           GHCJS.DOM.Element (getClientWidth)
import           Reflex
import           Reflex.Dom

import           Redoak.Editor
import           Redoak.Event
import           Redoak.Layout
import           Redoak.Layout.Identity
import           Redoak.Rectangle
import           Redoak.Language
import           Redoak.View


divId :: MonadWidget t m => String -> m a -> m a
divId id = elAttr "div" ("id" =: id)

divId' :: MonadWidget t m => String -> m () -> m (El t)
divId' id = fmap fst . elAttr' "div" ("id" =: id)

spanId :: MonadWidget t m => String -> m a -> m a
spanId id = elAttr "span" ("id" =: id)

editor :: MonadWidget t m => Event t (NonEmpty KeyEvent) -> m ()
editor keys = divId "editor" $ do
  (cursors, stati, widgets, debug) <- do
    states <- mapDyn splitMultiplexed =<< foldDyn handleEvents initState keys
    (,,,) <$> mapDyn (\(d,_,_,_) -> d) states
          <*> mapDyn (\(_,d,_,_) -> d) states
          <*> mapDyn (\(_,_,d,_) -> d) states
          <*> mapDyn (\(_,_,_,d) -> d) states

  rec (resizeEvent, contentEl) <- resizeDetectorWithStyle
                                  -- TODO replace 93 with something like "availible"
                                  "width: 100%; height: 93%; margin: 0; padding 0"
                                  $ divId' "content" $ do
        dimensionsDyn <- do
          let getDim = getClientWidth $ _el_element contentEl
              getDimActions = flip fmap resizeEvent $ \() -> getDim
          widgetHold (pure $ 99999999) getDimActions
        zipped <- combineDyn (,) dimensionsDyn cursors
        let layout' (w, c) = layout (W $ floor $ w / 15) c
        _ <- dyn =<< mapDyn (makeNode . runIdentity . layout') zipped
        el "div" $ dynText debug
        return ()
  elAttr "footer" ("id" =: "status") $ do
    let pathString (is, tip) =
          Data.List.intercalate ", " $ (fmap show is) ++
          [case tip of
              Single pos -> show pos
              Range (start, end) ->
                show start ++ if start == end then "" else "-" ++ show end
          ]
    spanId "path" $ dynText =<< mapDyn (pathString . path) cursors
    spanId "mode" $ dynText =<< mapDyn T.unpack stati

  _ <- widgetHoldInternal (pure ()) $ updated widgets

  return ()

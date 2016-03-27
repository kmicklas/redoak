{-# LANGUAGE CPP #-}

module Redoak.Setup
  ( withDocument
  ) where

import Data.Maybe
import GHCJS.DOM (WebView, runWebGUI, webViewGetDomDocument)
import GHCJS.DOM.Document (Document)
import System.Environment

#ifndef __GHCJS__
import GHCJS.DOM.Document (createElement, getHead)
import GHCJS.DOM.HTMLLinkElement (castToHTMLLinkElement, setRel, setHref)
import GHCJS.DOM.Node (appendChild)
import Graphics.UI.Gtk.WebKit.WebView (webViewGetWebSettings, webViewSetWebSettings)
import System.Glib.Attributes (AttrOp((:=)), set)
import System.Glib.Properties (newAttrFromBoolProperty)

import Paths_redoak
#endif

withDocument :: (WebView -> Document -> IO ()) -> IO ()
withDocument main = wrap $ runWebGUI $ \ view -> do
  Just doc <- webViewGetDomDocument view
#ifndef __GHCJS__
  settings <- webViewGetWebSettings view
  let fileAccess = newAttrFromBoolProperty "enable-file-access-from-file-uris"
  set settings [fileAccess := True]
  webViewSetWebSettings view settings

  Just head <- getHead doc
  Just link <- createElement doc $ Just "link"
  let style = castToHTMLLinkElement link
  setRel style "stylesheet"
  styleFile <- getDataUrl "style/redoak.css"
  setHref style styleFile
  appendChild head $ Just style
#endif
  main view doc
  where
#ifdef __GHCJS__
    wrap = id
#else
    wrap a = do
      url <- getDataUrl "native.html"
      withArgs [url] a
#endif

getDataUrl :: String -> IO String
#ifdef __GHCJS__
getDataUrl = return
#else
getDataUrl = fmap ("file://" ++) . getDataFileName
#endif

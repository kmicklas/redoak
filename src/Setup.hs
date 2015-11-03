{-# LANGUAGE CPP #-}

module Setup 
  ( setup
  ) where

import GHCJS.DOM (WebView, runWebGUI, webViewGetDomDocument, enableInspector)
import GHCJS.DOM.Document (Document, createElement, getHead)
import GHCJS.DOM.HTMLLinkElement (castToHTMLLinkElement, setRel, setHref)
import GHCJS.DOM.Node (appendChild)
import System.Environment

#ifndef __GHCJS__
import Graphics.UI.Gtk.WebKit.WebView (webViewGetWebSettings, webViewSetWebSettings, webViewLoadUri)
import System.Glib.Attributes (AttrOp((:=)), set)
import System.Glib.Properties (newAttrFromBoolProperty)

import Paths_redoak
#endif

setup :: (Document -> IO ()) -> IO ()
setup main = wrap $ runWebGUI $ \ view -> do
#ifndef __GHCJS__
  enableInspector view
  settings <- webViewGetWebSettings view
  let fileAccess = newAttrFromBoolProperty "enable-file-access-from-file-uris"
  set settings [fileAccess := True]
  webViewSetWebSettings view settings
#endif
  Just doc <- webViewGetDomDocument view
  Just head <- getHead doc
  Just link <- createElement doc $ Just "link"
  let style = castToHTMLLinkElement link
  setRel style "stylesheet"
  styleFile <- getDataURL "style/redoak.css"
  setHref style styleFile
  appendChild head $ Just style
  putStrLn styleFile
  main doc
  where
#ifdef __GHCJS__
    wrap = id
#else
    wrap a = do
      url <- getDataURL "empty.html"
      withArgs [url] a
#endif

getDataURL :: String -> IO String
#ifdef __GHCJS__
getDataURL = return
#else
getDataURL = fmap ("file://" ++) . getDataFileName
#endif

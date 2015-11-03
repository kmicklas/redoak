{-# LANGUAGE CPP #-}

module Setup 
  ( setup
  ) where

import GHCJS.DOM (WebView, webViewGetDomDocument, enableInspector)
import GHCJS.DOM.Document (Document, createElement, getHead)
import GHCJS.DOM.HTMLLinkElement (castToHTMLLinkElement, setRel, setHref)
import GHCJS.DOM.Node (appendChild)

#ifndef __GHCJS__
import Graphics.UI.Gtk.WebKit.WebView (webViewGetWebSettings, webViewSetWebSettings, webViewLoadUri)
import System.Glib.Attributes (AttrOp((:=)), set)
import System.Glib.Properties (newAttrFromBoolProperty)

import Paths_redoak
#endif

setup :: WebView -> IO ()
setup view = do
#ifndef __GHCJS__
  enableInspector view
  settings <- webViewGetWebSettings view
  let fileAccess = newAttrFromBoolProperty "enable-file-access-from-file-uris"
  set settings [fileAccess := True]
  webViewSetWebSettings view settings
  emptyFile <- getDataURL "empty.html"
  putStrLn emptyFile
  webViewLoadUri view emptyFile
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
  return ()

getDataURL :: String -> IO String
#ifdef __GHCJS__
getDataURL = return
#else
getDataURL = fmap ("file://" ++) . getDataFileName
#endif

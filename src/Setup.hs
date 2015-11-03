{-# LANGUAGE CPP #-}

module Setup 
  ( withDocument
  ) where

import Data.Maybe
import GHCJS.DOM (WebView, runWebGUI, webViewGetDomDocument)
import GHCJS.DOM.Document (Document)
import System.Environment

#ifndef __GHCJS__
import Graphics.UI.Gtk.WebKit.WebView (webViewGetWebSettings, webViewSetWebSettings)
import System.Glib.Attributes (AttrOp((:=)), set)
import System.Glib.Properties (newAttrFromBoolProperty)

import Paths_redoak
#endif

withDocument :: (Document -> IO ()) -> IO ()
withDocument main = wrap $ runWebGUI $ \ view -> do
#ifndef __GHCJS__
  settings <- webViewGetWebSettings view
  let fileAccess = newAttrFromBoolProperty "enable-file-access-from-file-uris"
  set settings [fileAccess := True]
  webViewSetWebSettings view settings
#endif
  webViewGetDomDocument view >>= (main . fromJust)
  where
#ifdef __GHCJS__
    wrap = id
#else
    wrap a = do
      url <- getDataURL "native.html"
      withArgs [url] a
#endif

getDataURL :: String -> IO String
#ifdef __GHCJS__
getDataURL = return
#else
getDataURL = fmap ("file://" ++) . getDataFileName
#endif

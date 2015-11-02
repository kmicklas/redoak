{-# LANGUAGE CPP #-}

module Setup 
  ( setup
  ) where

import GHCJS.DOM.Document (Document, createElement, getHead)
import GHCJS.DOM.HTMLLinkElement (castToHTMLLinkElement, setRel, setHref)
import GHCJS.DOM.Node (appendChild)

#ifndef __GHCJS__
import Paths_redoak
#endif

setup :: Document -> IO ()
setup doc = do
  Just head <- getHead doc
  Just link <- createElement doc $ Just "link"
  let style = castToHTMLLinkElement link
  setRel style "stylesheet"
  styleFile <- getDataFileName "style/redoak.css"
  setHref style styleFile
  appendChild head $ Just style
  putStrLn styleFile
  return ()

#ifdef __GHCJS__
getDataFileName = return
#endif

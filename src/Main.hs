module Main where

import Control.Applicative ((<$>))

import GHCJS.DOM (enableInspector, webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (getBody, createElement, click)
import GHCJS.DOM.HTMLElement (setInnerText)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.HTMLParagraphElement (castToHTMLParagraphElement)
import GHCJS.DOM.Node (appendChild)
import GHCJS.DOM.EventM (on, mouseClientXY)

import View
import Tree
import Editor
import UI

main = runWebGUI $ \ webView -> do
  enableInspector webView
  Just doc <- webViewGetDomDocument webView
  runEditor doc

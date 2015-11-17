{-# LANGUAGE ImplicitParams #-}

module Dom
  ( textNode
  , el
  ) where

import Control.Monad
import Data.Text
import GHCJS.DOM.Document (Document, createTextNode, createElement)
import GHCJS.DOM.Element (Element, setAttribute)
import GHCJS.DOM.Node (Node, toNode, appendChild)

textNode :: (?doc :: Document) => Text -> IO Node
textNode s = do
  Just n <- createTextNode ?doc s
  return $ toNode n

el :: (?doc :: Document) => Text -> [(Text,Text)] -> [Node] -> IO Node
el tag attrs inners = do
  Just parent <- createElement ?doc $ Just tag
  forM_ inners $ appendChild parent . Just
  forM_ attrs $ \ (k, v) -> setAttribute parent k v
  return $ toNode parent

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Dom
  ( WithDoc
  , textNode
  , el
  ) where

import Control.Monad
import Data.Text
import GHCJS.DOM.Document (Document, createTextNode, createElement)
import GHCJS.DOM.Element (Element, setAttribute)
import GHCJS.DOM.Node (Node, toNode, appendChild)

type WithDoc a = (?doc :: Document) => IO a

textNode :: Text -> WithDoc Node
textNode s = do
  Just n <- createTextNode ?doc s
  return $ toNode n

el :: Text -> [(Text,Text)] -> [Node] -> WithDoc Node
el tag attrs inners = do
  Just parent <- createElement ?doc $ Just tag
  forM_ inners $ appendChild parent . Just
  forM_ attrs $ \ (k, v) -> setAttribute parent k v
  return $ toNode parent

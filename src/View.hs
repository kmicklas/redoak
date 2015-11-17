{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module View 
  ( Element(..)
  , View
  , effectView
  ) where

import Control.Monad
import Data.Foldable
import Data.Text hiding (map)
import Data.Sequence
import GHCJS.DOM.Document (Document, getBody, getElementById)
import GHCJS.DOM.Node (Node, toNode, appendChild, getParentNode, removeChild)

import Dom
import React
import Tree

type View = Element (Text, [Text]) Text

effectView :: (?doc :: Document) => View -> IO ()
effectView new = do
  view <- makeNode new
  Just body <- getBody ?doc
  old <- getElementById ?doc (fst $ ident new)
  maybe (return ()) (removeNode . toNode) old
  appendChild body $ Just view
  return ()

removeNode :: Node -> IO ()
removeNode node = do
  Just parent <- getParentNode node
  removeChild parent $ Just node
  return ()

makeNode :: (?doc :: Document) => View -> IO Node
makeNode (Atom (id, cs) t) = el "span" (attrs id cs) =<< mapM textNode [t]
makeNode (Node (id, cs) es) = el "div" (attrs id cs) =<< mapM makeNode (toList es)

attrs :: Text -> [Text] -> [(Text, Text)]
attrs id classes = [("id", id), ("class", intercalate " " classes)]

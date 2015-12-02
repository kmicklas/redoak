{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module View 
  ( ViewInfo(..)
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
import Rectangle
import Tree

data ViewInfo
  = ViewInfo
    { ident :: Text
    , classes :: [Text]
    , dim :: Dimensions
    , pos :: Position
    }
  deriving (Eq, Ord, Show)

type View = Tree Text ViewInfo

effectView :: View -> WithDoc ()
effectView new = do
  view <- makeNode new
  Just body <- getBody ?doc
  old <- getElementById ?doc (ident $ ann new)
  maybe (return ()) (removeNode . toNode) old
  appendChild body $ Just view
  return ()

removeNode :: Node -> IO ()
removeNode node = do
  Just parent <- getParentNode node
  removeChild parent $ Just node
  return ()

makeNode :: View -> WithDoc Node
makeNode (ViewInfo id cs _ _ := Atom t) =
  el "span" (attrs id cs) =<< mapM textNode [t]
makeNode (ViewInfo id cs _ _ := Node es) =
  el "div" (attrs id cs) =<< mapM makeNode (toList es)

attrs :: Text -> [Text] -> [(Text, Text)]
attrs id classes = [("id", id), ("class", intercalate " " classes)]

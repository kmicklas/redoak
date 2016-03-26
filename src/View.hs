{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImplicitParams #-}
--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module View
  ( ViewInfo(..)
  , View
  , makeNode
  ) where

import           Control.Monad
import           Data.Foldable
import           Data.List (intercalate)
import qualified Data.Map as M
import           Data.MonoTraversable
import           Data.Sequence hiding ((:<))
import           Data.Text (Text)
import qualified Data.Text as T
import           GHCJS.DOM (WebView)
import           GHCJS.DOM.Document (Document, getBody, getElementById)
import           GHCJS.DOM.Node (Node, toNode, appendChild, getParentNode, removeChild)
import           Reflex.Dom

import           Rectangle
import           Tree

data ViewInfo
  = ViewInfo
    { ident :: Maybe Text
    , classes :: [Text]
    , dim :: Dimensions
    , pos :: Position
    }
  deriving (Eq, Ord, Show)

type View = Tree Text ViewInfo

makeNode :: MonadWidget t m => View -> m ()
makeNode (ViewInfo id cs _ _ :< e) = elAttr typ as $ case e of
    Atom t  -> text $ T.unpack t
    Node es -> mapM_ makeNode es
  where typ = case e of
          Atom t  -> "span"
          Node es -> "div"
        as = attrs (T.unpack <$> id) (T.unpack <$> cs)

attrs :: Maybe String -> [String] -> AttributeMap
attrs id classes = M.fromList $
  [("class", intercalate " " classes)] ++ maybe [] (return . ("id",)) id

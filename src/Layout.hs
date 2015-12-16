{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

module Layout where

import Control.Monad
import Control.Monad.Identity
import Data.Bifunctor
import Data.Foldable
import Data.Sequence
import Data.Sequences as SS
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Traversable
import Prelude   hiding (foldr)

import Rectangle
import Tree
import View

-- MAGIC CONSTANTS:
indentWidth = W 50
maxInlineHeight = H 50
inlinePad = W 20

data LayoutInfo
  = LayoutInfo
    { ident :: Maybe Text
    , selection :: Maybe Range
    --, alwaysBreak :: Bool
    --, headed :: Bool -- is first child special
    --, group :: Bool  -- is this an element from the original tree
    }
  deriving (Eq, Ord, Show)

type Layout    = Tree Text LayoutInfo
type LayoutDim = Tree Text (LayoutInfo, Dimensions)

data Direction
  = Vertical
  | Horizontal
  deriving (Eq, Ord, Show)

class (Monad r) => Rules r where
  inlineText :: Text -> r Dimensions
  availableWidth :: r Width

instance Rules Identity where
  inlineText t = return (W $ T.length t * 20, maxInlineHeight)
  availableWidth = return $ W 500

layout :: Rules r => Cursor Text Word -> r View
layout c = do
  w <- availableWidth
  layoutFull w <$> computeFull (makeLayout c)

makeLayout :: Cursor Text Word -> Layout
makeLayout = layoutWithSelection . findPath True
  where layoutWithSelection :: Tree Text ((Word, Selection), Bool) -> Layout
        layoutWithSelection = second $ \ ((id, sel), onPath) -> LayoutInfo
          { Layout.ident = Just $ pack $ show id
          , Layout.selection = case (onPath, sel) of
              (True, Select r) -> Just r
              _                -> Nothing
          }

        findPath :: Bool
                 -> Cursor Text Word
                 -> Tree Text ((Word, Selection), Bool)
        findPath onPath (T (a@(_, sel) := e)) = T $ ((a, onPath) :=) $ case e of
          Atom a  -> Atom a
          Node ts -> Node $ case sel of
            Select  _ -> findPath False <$> ts
            Descend i ->
              fmap (uncurry findPath)
                $ fmap (first (== i))
                $ snd
                $ mapAccumL (\ count elem -> (count + 1, (count, elem))) 0 ts

computeFull :: Rules r => Layout -> r LayoutDim
computeFull (T (info := e)) = do
    (dim, e') <- case e of
      Atom a -> do
        (, Atom a) <$> inlineText a
      Node ts -> do
        fulls <- mapM computeFull ts
        let fullDims = fmap (snd . ann . unTree) fulls
        let maxWidth  = maximum $ W 0 <| fmap fst fullDims
        let maxHeight = maximum $ H 0 <| fmap snd fullDims
        let dim = if maxHeight <= maxInlineHeight
                  then (sum $ fmap ((+ inlinePad) . fst) fullDims, maxHeight)
                  else (maxWidth, sum $ fmap snd fullDims)
        return (dim, Node fulls)
    return $ T $ (info, dim) := e'

-- all nodes must have full size computed
layoutFull :: Width -> LayoutDim -> View
layoutFull mw t@(T ((info, (w, h)) := e)) =
  if w <= mw
  then defaultView Horizontal
  else case e of
    Atom a -> defaultView Vertical
    Node ts ->
      case viewl ts of
        EmptyL -> defaultView Vertical
        first :< rest ->
          let views = layoutFull mw first
                   <| fmap (layoutFull (mw - indentWidth)) rest in
          let fullDim = ( maximum $ W 0 <| fmap (fst . dim . ann . unTree) views
                        , sum $ fmap (snd . dim . ann . unTree) views
                        ) in
          sel $ T $ makeViewInfo Vertical (info, fullDim) := Node views

  where sel = select $ selection info
        defaultView dir = sel $ second (makeViewInfo dir) t

select :: Maybe Range -> View -> View
select Nothing t = t
select (Just (start, end)) (T (info := e)) = T $ (info :=) $ case e of
  Atom a -> Node $
    [ T $ lInfo := Atom lPart
    , T $ selInfo := Atom selPart
    , T $ rInfo := Atom rPart
    ]
    where (lPart, selPart, rPart) = split a
          lInfo = ViewInfo
            { View.ident = Nothing
            , classes = []
            , dim = (W 0, H 0)
            , pos = (X 0, Y 0)
            }
          rInfo = lInfo

  Node ts -> Node $ mconcat [lPart, [T $ selInfo := Node selPart], rPart]
    where (lPart, selPart, rPart) = split ts

  where front = min start end
        back  = max start end
        selInfo = ViewInfo
          { View.ident = Just "selection"
          , classes = []
          , dim = (W 0, H 0)
          , pos = (X 0, Y 0)
          }
        split :: IsSequence s => s -> (s, s, s)
        split s =
          ( SS.take (fromIntegral front) s
          , SS.take (fromIntegral $ back - front) $
            SS.drop (fromIntegral front) s
          , SS.drop (fromIntegral back) s
          )

dirClass :: Direction -> Text
dirClass Horizontal = "horizontal"
dirClass Vertical   = "vertical"

makeViewInfo :: Direction -> (LayoutInfo, Dimensions) -> ViewInfo
makeViewInfo dir (LayoutInfo id sel, dim) = ViewInfo
  { View.ident = id
  , classes = ["content", dirClass dir]
  , dim = dim
  , pos = (X 0, Y 0)
  }

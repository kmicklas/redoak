{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

module Redoak.Layout where

import           Control.Monad
import           Control.Monad.Identity
import           Data.Bifunctor
import           Data.Foldable
import           Data.Sequence hiding ((:<))
import qualified Data.Sequence as S
import           Data.Sequences as SS
import           Data.Text (Text, pack)
import qualified Data.Text as T
import           Data.Traversable
import           Prelude hiding (foldr)

import           Redoak.Rectangle
import           Redoak.Tree
import           Redoak.Tree.Range
import           Redoak.View

-- MAGIC CONSTANTS:
indentWidth = W 8
maxInlineHeight = H 1
inlinePad = W 0

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

instance Rules Identity where
  inlineText t = return (W $ T.length t, maxInlineHeight)

layout :: Rules r => Width -> Cursor Text Word -> r View
layout maxWidth c = do
  layoutFull maxWidth <$> computeFull (makeLayout c)

makeLayout :: Cursor Text Word -> Layout
makeLayout = layoutWithSelection . findPath True
  where layoutWithSelection :: Tree Text ((Word, Selection), Bool) -> Layout
        layoutWithSelection = fmap $ \ ((id, sel), onPath) -> LayoutInfo
          { Redoak.Layout.ident = Just $ pack $ show id
          , Redoak.Layout.selection = case (onPath, sel) of
              (True, Select r) -> Just r
              _                -> Nothing
          }

        findPath :: Bool
                 -> Cursor Text Word
                 -> Tree Text ((Word, Selection), Bool)
        findPath onPath (a@(_, sel) :< e) = ((a, onPath) :<) $ case e of
          Atom a  -> Atom a
          Node ts -> Node $ case (onPath, sel) of
            (True, Descend i) -> fmap (uncurry findPath)
                                   $ fmap (first (== i))
                                   $ snd
                                   $ mapAccumL (\ count elem -> (count + 1, (count, elem))) 0 ts
            _                 -> findPath False <$> ts

computeFull :: Rules r => Layout -> r LayoutDim
computeFull (info :< e) = do
    (dim, e') <- case e of
      Atom a -> do
        (, Atom a) <$> inlineText a
      Node ts -> do
        fulls <- mapM computeFull ts
        let fullDims = fmap (snd . ann) fulls
        let maxWidth  = maximum $ W 0 <| fmap fst fullDims
        let maxHeight = maximum $ H 0 <| fmap snd fullDims
        let dim = if maxHeight <= maxInlineHeight
                  then (sum $ fmap ((+ inlinePad) . fst) fullDims, maxHeight)
                  else (maxWidth, sum $ fmap snd fullDims)
        return (dim, Node fulls)
    return $ (info, dim) :< e'

-- all nodes must have full size computed
layoutFull :: Width -> LayoutDim -> View
layoutFull mw t@((info, (w, h)) :< e) =
  if w <= mw
  then defaultView Horizontal
  else case e of
    Atom a -> defaultView Vertical
    Node ts ->
      case viewl ts of
        EmptyL -> defaultView Vertical
        (S.:<) first rest ->
          let views = layoutFull mw first
                   <| fmap (layoutFull (mw - indentWidth)) rest in
          let fullDim = ( maximum $ W 0 <| fmap (fst . dim . ann) views
                        , sum $ fmap (snd . dim . ann) views
                        ) in
          sel $ makeViewInfo Vertical (info, fullDim) :< Node views

  where sel = select $ selection info
        defaultView dir = layoutHomogenous dir t

layoutHomogenous :: Direction -> LayoutDim -> View
layoutHomogenous dir (ann@(info, _) :< e) =
  select (selection info)
    $ makeViewInfo dir ann :< second (layoutHomogenous dir) e

select :: Maybe Range -> View -> View
select Nothing t = t
select (Just (start, end)) (info :< e) = uncurry (:<) $ case e of
  Atom a -> (info',) $ Node $
    [ lInfo :< Atom lPart
    , selInfo :< Atom selPart
    , rInfo :< Atom rPart
    ]
    where (lPart, selPart, rPart) = split a
          lInfo = ViewInfo
            { Redoak.View.ident = Nothing
            , classes = []
            , dim = (W 0, H 0)
            , pos = (X 0, Y 0)
            }
          rInfo = lInfo
          info' = info { classes = ["atom-selection"] }

  Node ts ->
    (info, Node $ mconcat [lPart, [selInfo :< Node selPart], rPart])
    where (lPart, selPart, rPart) = split ts

  where front = min start end
        back  = max start end
        selInfo = ViewInfo
          { Redoak.View.ident = Just "selection"
          , classes = [if start <= end then "right" else "left"]
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
  { Redoak.View.ident = id
  , classes = ["content", dirClass dir]
  , dim = dim
  , pos = (X 0, Y 0)
  }

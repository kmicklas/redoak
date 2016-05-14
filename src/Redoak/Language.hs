{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Redoak.Language
  ( module Redoak.Language.Base
  , module Redoak.Language.Cursor
  , module Redoak.Language.Hole
  , Term(..)
  , RootTerm(..)
  , Language(..)
  ) where

import Data.Text (Text)
import Reflex
import Reflex.Dom

import Redoak.Event
import Redoak.Language.Base
import Redoak.Language.Cursor
import Redoak.Language.Hole

type Term f0 f1 f2 f3 f4 f5 f6 f7 n =
  Cursor
    f0 f1 f2 f3 f4 f5 f6 f7
    n
    (Ann f0 f1 f2 f3 f4 f5 f6 f7)

type RootTerm f0 f1 f2 f3 f4 f5 f6 f7 = Term f0 f1 f2 f3 f4 f5 f6 f7 7

class NonTerminalAll f0 f1 f2 f3 f4 f5 f6 f7
      => Language f0 f1 f2 f3 f4 f5 f6 f7 where

  type Ann f0 f1 f2 f3 f4 f5 f6 f7 :: *

  type Accum f0 f1 f2 f3 f4 f5 f6 f7 :: *

  handleEvent :: KeyEvent
              -> ( RootTerm  f0 f1 f2 f3 f4 f5 f6 f7
                 , Accum     f0 f1 f2 f3 f4 f5 f6 f7)
              -> ( RootTerm  f0 f1 f2 f3 f4 f5 f6 f7
                 , Accum     f0 f1 f2 f3 f4 f5 f6 f7)


  getMessage :: MonadWidget t m
             => ( RootTerm  f0 f1 f2 f3 f4 f5 f6 f7
                , Accum     f0 f1 f2 f3 f4 f5 f6 f7)
             -> ( Text  -- mode/status/whatever
                , m ()) -- some widget to display
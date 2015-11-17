{-# LANGUAGE ImplicitParams #-}

module Main where

import Editor
import Setup
import Tree
import UI
import View

main = withDocument $ \ doc -> let ?doc = doc in runEditor

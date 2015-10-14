module React
  ( react
  ) where

import Control.Concurrent.MVar

react :: MVar e -> s -> (e -> s -> s) -> (s -> IO ()) -> IO ()
react events init update effect = do
  event <- takeMVar events
  let state = update event init
  effect state
  react events state update effect

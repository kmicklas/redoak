module React
  ( react
  ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class

react :: (MonadIO m) =>
  MVar e -> s -> (e -> s -> s) -> (s -> m ()) -> m ()
react events init update effect = do
  event <- liftIO $ takeMVar events
  let state = update event init
  effect state
  react events state update effect

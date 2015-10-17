module React
  ( react
  ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class

react :: (MonadIO m) =>
  MVar e -> s -> (e -> s -> s) -> (s -> m ()) -> m a
react events init update effect = do
  effect init
  event <- liftIO $ takeMVar events
  react events (update event init) update effect

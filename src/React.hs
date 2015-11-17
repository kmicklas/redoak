module React
  ( react
  ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class

react :: (MonadIO m) =>
  MVar e -> (e -> s -> s) -> (s -> m ()) -> s -> m a
react events update effect = loop
  where loop state = do
          effect state
          event <- liftIO $ takeMVar events
          loop $ update event state

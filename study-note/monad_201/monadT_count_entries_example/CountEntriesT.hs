module CountEntriesT (countEntries) where

import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad (forM_, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell, runWriterT)

-- WriterT :: m (a, w) -> WriterT w m a
-- runWriterT :: WriterT w m a -> m (a, w)
-- execWriterT :: Monad m => WriterT w m a -> m w

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries newName
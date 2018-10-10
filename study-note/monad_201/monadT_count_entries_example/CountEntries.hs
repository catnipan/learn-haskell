module CountEntries (listDirectory, countEntriesTrad) where

import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (FilePath, (</>))
import Control.Monad (forM, liftM)

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
    let newName = path </> name
    isDir <- doesDirectoryExist newName
    if isDir
      then countEntriesTrad newName
      else return []
  return $ (path, length contents) : concat rest

-- use WriterT to add the recording capability to IO

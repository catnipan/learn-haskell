import System.Environment
import System.Directory
import System.IO
import Data.List

-- main = do
--   args <- getArgs
--   progName <- getProgName
--   putStr "The arguments are:\t"
--   mapM putStrLn args
--   putStr "The program name is:\t"
--   putStrLn progName

-- 获取命令行及 cli 文件名
-- getArgs :: IO [String]
-- getProgName :: IO String

type HandleCli = [String] -> IO ()

dispatch :: [(String, HandleCli)]
dispatch = [("add", add),
            ("view", view),
            ("remove", remove),
            ("bump", bump)]

add :: HandleCli
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: HandleCli
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStr $ unlines numberedTasks

modifyFile :: String -> (Handle -> Handle -> IO a) -> IO a
modifyFile fileName f = do
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  result <- f handle tempHandle
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName
  return result

remove :: HandleCli
remove [fileName, idxString] = do
  modifyFile fileName (\handle tempHandle -> do
    contents <- hGetContents handle
    let idx = read idxString :: Int
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! idx) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    )

bump :: HandleCli
bump [fileName, idxString] = do
  modifyFile fileName (\handle tempHandle -> do
    contents <- hGetContents handle
    let idx = read idxString :: Int
        todoTasks = lines contents
        targetTask = todoTasks !! idx
        newTodoTasks = targetTask:(delete targetTask todoTasks)
    hPutStr tempHandle $ unlines newTodoTasks
    )

main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args
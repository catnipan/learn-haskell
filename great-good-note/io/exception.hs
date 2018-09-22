import System.Environment
import System.IO
import System.IO.Error
import System.Directory

-- div :: Integral a => a -> a -> a
-- 但是 4 `div` 0
-- *** Exception: divide by zero
-- 在该类型中无法映射到一个值
-- head []
-- *** Exception: Prelude.head: empty list

-- pure code 能丢出 Exception，但 Exception 只能在 I/O section 中被接到
-- 因为在 pure code 中你不知道什么东西什么时候会被 evaluate
-- lazy 特性的缘故使得程序没有一个特定的执行顺序，但 I/O code 有

main1 = do
  (fileName:_) <- getArgs
  fileExisits <- doesFileExist fileName
  if fileExisits
    then do
      contents <- readFile fileName
      putStrLn $ "The file has " ++ (show . length . lines) contents ++ " lines."
    else do
      putStrLn "The file does not exist."

-- 第一种方法：使用函数判断文件是否存在
-- doesFileExist :: FilePath -> IO Bool

-- 第二种方法：使用 exception
-- catchIOError :: IO a -> (IOError -> IO a) -> IO a

main = toTry `catchIOError` handler

toTry :: IO ()
toTry = do
  (fileName:_) <- getArgs
  contents <- readFile fileName
  putStrLn $ "The file has " ++ (show . length . lines) contents ++ " lines"

handler :: IOError -> IO ()
handler e
  | isDoesNotExistError e =
    case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File doest not exist"
                             Nothing -> putStrLn $ "No file path!"
  | otherwise = ioError e

-- isDoesNotExistError :: IOError -> Bool
-- 只接住文件不存在的 IOError
-- ioError :: IOError -> IO a
-- Raise an IOError in the IO monad.

-- main3 = do
--   toTry1 `catchIOError` handler1
--   toTry2 `catchIOError` handler2
-- 分别 catchIOError
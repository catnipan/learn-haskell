import Control.Monad
import Data.Char
import System.IO

-- ghc --make capslocker

main0 = forever $ do
  l <- getLine
  putStrLn $ map toUpper l

-- getContents 是一个从标准输入读取直到 EOF 字符的 I/O action
-- getContents :: IO String
-- 惰性 IO，一行一行读入，因为输出才是真正需要输入的时候

main1 = do
  contents <- getContents
  putStr (map toUpper contents)

main2 = do
  contents <- getContents
  putStr $ shortLinesOnly contents

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines

interact' :: (String -> String) -> IO ()
interact' func = do
  contents <- getContents
  putStr $ func contents

-- main2 等价于
main3 = interact' shortLinesOnly

respondPalindromes = unlines . map (\xs -> 
  if isPalindrome xs then "palindrome" else "not a palindrome") . lines
  where isPalindrome xs = xs == reverse xs

main4 = interact respondPalindromes

main5 filePath = do
  handle <- openFile filePath ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

-- type FilePath = String
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
-- openFile :: FilePath -> IOMode -> IO Handle
-- hGetContents :: Handle -> IO String
-- hClose :: Handle -> IO ()

-- 等价于

withFile' :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile' path mode f = do
  handle <- openFile path mode
  result <- f handle
  hClose handle
  return result

main6 :: FilePath -> IO ()
main6 filePath = do
  withFile' filePath ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr contents)

-- hGetLine :: Handle -> IO String
-- hPutChar :: Handle -> Char -> IO ()
-- hPutStr :: Handle -> String -> IO ()
-- hPutStrLn :: Handle -> String -> IO ()
-- hGetChar :: Handle -> IO Char

-- readFile :: FilePath -> IO String

main7 filePath = do
  contents <- readFile filePath
  putStr contents

-- writeFile :: FilePath -> String -> IO ()
-- appendFile :: FilePath -> String -> IO ()
-- appendFile 不会覆盖，在后面添加

main8 filePath = do
  contents <- readFile filePath
  writeFile ("_" ++ filePath) (map toUpper contents)

main9 = do
  todoItem <- getLine
  appendFile "todo.txt" (todoItem ++ "\n")

main10 filePath = do
  withFile filePath ReadMode (\handle -> do
    hSetBuffering handle $ BlockBuffering (Just 2048)
    contents <- hGetContents handle
    putStr contents)

-- hSetBuffering :: Handle -> BufferMode -> IO ()
-- data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)
-- NoBuffering 一次读一个 character
-- LineBuffering 一次读一行
-- BlockBuffering (Just ___) 自定义
-- BlockBuffering Nothing 操作系统自动决定
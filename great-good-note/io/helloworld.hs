import Data.Char
import Control.Monad

main0 = putStrLn "hello, world"

-- ghc --make helloworld

-- putStrLn :: String -> IO ()
-- putStrLn 接受一个字符串并返回一个 I/O action
-- 这个 I/O action 包含了 () 的类型
-- 一个I/O action 是一个会造成副作用的动作，同时也代表会返回某些值

main1 = do
  foo <- putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn ("Hey " ++ name ++ " , you rock!")

-- 每一步都是一个 I/O action
-- 将所有 I/O action 用 do 连起来成为一个大的 I/O action

-- getLine :: IO String
-- name <- getLine
-- 执行一个名为 getLine 的 I/O action，并把它的结构绑定到 name
-- 因为 getLine :: IO String ，所以 name :: String

-- 每个 I/O action 都有一个值封装在里面
-- 因为 putStrLn "Hello, what's your name" :: IO ()
-- 所以 foo 只会有一个 () 值，空的 tuple，没有意义

-- 注意最后一行没有绑定任何名字
-- do block 会自动从最后一个 action 取出值并绑定给它的结果

-- 从 main 开始执行 I/O action
-- 或者在 GHCi 中打几个数字或是调用一个函数，按下 Enter 会计算它并调用 show，再用 putStrLn 将字符串印出在终端上

-- let bindings in expression
-- 其中 in 的 部分不是必须的

main2 = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

main3 = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      main3

-- 这里 if __ then __ else __ 表达式返回的都是 IO () 类型
-- return 的意义是利用某个 pure value 造出 I/O action
-- 将一个 value 装进“箱子”里面，产出的 I/O action 不做任何事

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

main4 = do
  return ()
  return ("HAHA")
  line <- getLine
  return "BLAH BLAH BLAH"
  return 4
  putStrLn line

-- 以上 return () 没有造成任何影响

main5 = do
  a <- return "hell" -- 等价于 a = "hell"
  b <- return "yeah!"
  putStrLn $ a ++ " " ++ b

-- return ___ 和 ___ <- 相反
-- 一个是装进盒子，一个是从盒子里拿出来

main6 = do putStr "Hey,"
           putStr "I'm "
           putChar 'A'
           putStrLn "ndy!"

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
  putChar x
  putStr' xs

putStrLn' :: String -> IO ()
putStrLn' str = do
  putStr' str
  putChar '\n'

print' :: (Show a) => a -> IO ()
print' = putStrLn' . show

main7 = do
  c <- getChar
  if c /= ' '
    then do
      putChar c
      main7
    else return ()

when' :: Bool -> IO () -> IO ()
when' val io =
  if val
    then io
    else return ()

main8 = do
  c <- getChar
  when' (c /= ' ') $ do
    putChar c
    main8

-- sequence 接受一连串 I/O action
-- sequence :: [IO a] -> IO [a]
main9 = do
  a <- getLine
  b <- getLine
  c <- getLine
  print [a, b, c]

sequence' :: [IO a] -> IO [a]
sequence' (io:ios) = do
  ioResult <- io
  iosResult <- sequence' ios
  return (ioResult:iosResult)
sequence' _ = return []

-- 等价于
main9' = do
  rs <- sequence' [getLine, getLine, getLine]
  print rs

-- sequence (map print [1,2,3,4,5])
-- 1
-- 2
-- 3
-- 4
-- 5
-- [(),(),(),(),()]

mapM' :: (a -> IO b) -> [a] -> IO [b]
mapM' func list = sequence' $ map func list
-- mapM' print [1,2,3]
-- 1
-- 2
-- 3
-- [(),(),()]
-- 最后打印出函数返回的结果，即 [(), (), ()]

mapM_' :: (a -> IO b) -> [a] -> IO ()
mapM_' func list = do
  mapM' func list
  return ()
-- mapM_' print [1..4]
-- 1
-- 2
-- 3
-- 4
-- 结果是 () 的时候不会在 GHCi 中打印出来

forever' :: IO a -> IO a
forever' io = do
  io
  forever' io

main10 = forever' $ do
  putStr "Give me some input: "
  l <- getLine
  putStrLn $ map toUpper l

forM' :: [a] -> (a -> IO b) -> IO [b]
forM' list genIo = sequence $ map genIo list

main11 = do
  colors <- forM' [1,2,3,4] (\a -> do
    putStrLn $ "Which color do you associate with the number " ++ show a ++ " ?"
    getLine) -- 和下面等价，拿出来在放回去
    -- color <- getLine
    -- return color)
  putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
  forM' colors putStrLn
  -- mapM putStrLn colors
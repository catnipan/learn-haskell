-- main = putStrLn "hello, world"

-- ghc --make helloworld

-- putStrLn :: String -> IO ()
-- putStrLn 接受一个字符串并返回一个 I/O action
-- 这个 I/O action 包含了 () 的类型
-- 一个I/O action 是一个会造成副作用的动作，同时也代表会返回某些值

main = do
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



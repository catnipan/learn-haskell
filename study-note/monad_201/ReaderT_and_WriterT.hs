-- https://gist.github.com/Decoherence/39a4c34685d86a334b63

import Control.Monad.Reader
import Control.Monad.Writer

data Person = Person { name :: String } deriving (Show)

alex = Person "Alex Fontaine"
philip = Person "Philip Carpenter"
kim = Person "Kim Lynch"

peopleDb = [alex, philip, kim]

-- get person from environment, log the person, and print name
process :: ReaderT Person (WriterT String IO) ()
process = do
  tell "Looking for a person."
  Person p <- ask
  tell $ "Found person: " ++ p ++ ". "
  liftIO $ putStrLn p


-- get person from environment, log the person, and return the name
process' :: ReaderT Person (WriterT String IO) String
-- process' = do
--   tell "Looking for a person... "
--   Person p <- ask
--   tell $ "Found person: " ++ p ++ ". "
--   return p
process' = (tell "Looking for a person... ") >>= (\_ ->
    ask >>= (\(Person p) -> 
      (tell $ "Found person: " ++ p ++ ". ") >>= (\_ -> return p)
      )
  )

process'' :: WriterT String (ReaderT Person IO) ()
process'' = do
  tell "Looking for a person."
  Person p <- ask
  tell $ "Found person: " ++ p ++ ". "
  liftIO $ putStrLn p

-- runWriterT (runReaderT process' alex)
-- runReaderT (runWriterT process'') alex

-- runWriterT (runReaderT process alex)
-- runWriterT (mapM (runReaderT process') peopleDb)


main1 :: IO ()
main1 = do
  result1 <- runWriterT (runReaderT process alex) -- :: ((), String)
  putStrLn $ snd result1

main2 :: IO ()
main2 = do
  result2 <- runWriterT (runReaderT process' alex)
  putStrLn $ fst result2
  putStrLn $ snd result2

main3 :: IO ()
main3 = do
  result3 <- runWriterT (mapM (runReaderT process') peopleDb)

  let people = fst result3
      log = snd result3
  
  putStrLn "\n\nReaderT values:\n"
  mapM_ putStrLn people

  putStrLn "\nWriterT log:\n"
  putStrLn $ log

  return ()
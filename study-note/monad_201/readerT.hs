import Control.Monad.Reader

data Person = Person { getAge :: Int, getName :: String, getGoToPartyOrNot :: Bool }
type Persons = [Person]

yifan :: Person
yifan = Person { getAge = 25, getName = "Yifan", getGoToPartyOrNot = True }

mary :: Person
mary = Person { getAge = 21, getName = "Mary", getGoToPartyOrNot = False }

getPersonTag :: ReaderT Person Maybe String
getPersonTag = do
  name <- asks' getName
  age <- local' (\p -> p { getAge = (getAge p + 1) }) (asks' getAge)
  isGotoTheParty <- asks' getGoToPartyOrNot
  ReaderT $ (\_ -> if isGotoTheParty then Just (show age ++ ": " ++ name) else Nothing)
  
local' :: (Monad m) => (e -> e) -> ReaderT e m r -> ReaderT e m r
local' localFunction readerX =
  ReaderT $ (\env -> (
    runReaderT readerX (localFunction env)
  ))

ask' :: (Monad m) => ReaderT e m e
ask' = ReaderT $ (\env -> return env)

asks' :: (Monad m) => (r -> a) -> ReaderT r m a
asks' f = ReaderT $ (\env -> return (f env))

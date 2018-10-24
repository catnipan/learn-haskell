import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just
  x >>= f = MaybeT $ do
    maybe_val <- runMaybeT x
    case maybe_val of
      Nothing -> return Nothing
      Just value -> runMaybeT $ f value

instance Monad m => Applicative (MaybeT m) where
  pure = return
  (<*>) = ap

instance Monad m => Functor (MaybeT m) where
  fmap = liftM

instance Monad m => Alternative (MaybeT m) where
  empty = MaybeT $ return Nothing
  x <|> y = MaybeT $ do
    maybe_val <- runMaybeT x
    case maybe_val of
      Nothing -> runMaybeT y
      Just _ -> return maybe_val

instance Monad m => MonadPlus (MaybeT m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans MaybeT where
  -- lift :: (MonadTrans t, Monad m) => m a -> t m a
  lift = MaybeT . (liftM Just)

-- liftM :: (Monad m) => (a1 -> r) -> m a1 -> m r
-- liftM f m1 = do { x1 <- m1; return (f x1) }

isValid :: String -> Bool
isValid str = length str > 10

getPassphrase :: MaybeT IO String
getPassphrase = do
  s <- lift getLine
  guard (isValid s)
  return s

-- lift getLine ??
-- getLine :: IO String
-- lift :: (MonadTrans t, Monad m) => m a -> t m a
-- m is IO, t is MaybeT
-- lift :: IO String -> MaybeT IO String
-- lift getLine = MaybeT $ liftM Just $ getLine

-- liftM :: (Monad m) => (a -> r) -> m a -> m r
-- liftM Just getLine
-- m is IO, a is String
-- liftM :: (String -> Just String) -> IO String -> IO (Just String)
-- liftM Just getLine = do { line <- getLine; return (Just line) }
-- liftM Just getLine :: IO (Maybe String)
-- lift getLine :: MaybeT IO String

-- guard           :: (Alternative f) => Bool -> f ()
-- guard True      =  pure ()
-- guard False     =  empty
-- f is MaybeT IO
-- pure = return :: a -> MaybeT IO a
-- pure () = return () = MaybeT . return . Just $ ()
-- empty :: MaybeT IO a
-- empty is MaybeT $ return Nothing
-- guard True is IO (Just ())
-- guard False is IO Nothing

askPasspharase :: MaybeT IO ()
askPasspharase = do
  lift $ putStrLn "Insert your new passphrase:"
  value <- getPassphrase
  lift $ putStrLn "Storing in database..."

asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldr (<|>) empty

-- ask the user ad infinitum for a valid passphrase:
askPasspharase' :: MaybeT IO ()
askPasspharase' = do
  lift $ putStrLn "Insert your new passphrase:"
  value <- msum $ repeat getPassphrase
  lift $ putStrLn "Storing in database..."
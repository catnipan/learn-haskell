import Html
import Prelude hiding (div)

newtype Reader e a = Reader { runReader :: e -> a }

instance Functor (Reader a) where
  fmap f (Reader g) = Reader $ f . g
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> Reader x a -> Reader x b

instance Applicative (Reader a) where
  pure x = Reader $ \_ -> x
  m <*> n = Reader $ \e -> (runReader m e) (runReader n e)
  
instance Monad (Reader a) where
  m >>= f = Reader $ \e -> runReader (f $ runReader m $ e) e

ask :: Reader a a
ask = Reader id

asks :: (e -> a) -> Reader e a
asks = Reader

-- Reader Email Html
-- we have a Reader whose “shared environmental state” is of type Email and when “run” with such a value, it will return a value of type Html
-- a direct analogue of Email -> Html

main :: IO ()
main =
  putStrLn "what is your email address?" >>
    getLine >>= \email ->
      putStrLn . show $ runReader view email

view :: Reader Email Html
view =
  page >>= \page' ->
    return $ div [ page' ]

page :: Reader Email Html
page =
  content >>= \content' ->
    return $ div [ topNav, content' ]
  
topNav :: Html
topNav = div [ h1 ["OurSite.com"] ]

content :: Reader Email Html
content =
  ask >>= \email ->
    right >>= \right' ->
      return $ div
        [ h1 [ "Custom Content for " ++ email], left, right' ]

left :: Html
left = div [ p [ "this is the left side." ] ]

right :: Reader Email Html
right =
  article >>= \article' ->
    return $ div [ article' ]

article :: Reader Email Html
article =
  widget >>= \widget' ->
    return $ div [ p ["this is an article"], widget' ]

widget :: Reader Email Html
widget =
  ask >>= \email ->
    return $ div
      [ p [ "Hey " ++ email ++ ", we've got a great offer for you!"] ]
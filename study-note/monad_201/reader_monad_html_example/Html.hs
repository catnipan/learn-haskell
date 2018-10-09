module Html where

import Control.Monad.Reader (Reader, ask, runReader)
import Prelude hiding (div)

type Html = String
type Email = String

data HtmlTag = DIV | H1 | P

genTagPair :: HtmlTag -> (String, String)
genTagPair htmlTag = ("<" ++ tag ++ ">", "</" ++ tag ++ ">")
  where tag = case htmlTag of
                DIV -> "div"
                H1 -> "h1"
                P -> "P"

wrapWithTag :: HtmlTag -> Html -> Html
wrapWithTag htmlTag html =
  let (f,b) = genTagPair htmlTag
  in f ++ html ++ b

div :: [Html] -> Html
div children = wrapWithTag DIV $ mconcat children

h1 :: [Html] -> Html
h1 children = wrapWithTag H1 $ mconcat children

p :: [Html] -> Html
p children = wrapWithTag P $ mconcat children

import Data.Char
import Data.Monoid

-- 检验是不是一样
data SameOrDifferent = Different | Same deriving (Show)

instance Monoid SameOrDifferent where
  mempty = Same
  Different `mappend` _ = Different
  Same `mappend` y = y

checkIsEqual :: (Eq a) => a -> a -> SameOrDifferent
checkIsEqual x y = if x == y then Same else Different

-- (1, "a", 34.5)
-- (2, "b", 344)
isTupleEqual :: (Eq a, Eq b, Eq c) => (a, b, c) -> (a, b, c) -> SameOrDifferent
isTupleEqual (x1, y1, z1) (x2, y2, z2) = mconcat
  [checkIsEqual x1 x2, checkIsEqual y1 y2, checkIsEqual z1 z2]


-- 表单的错误检验逻辑可以拆开：
-- （FieldError type, 下面的 mappend 函数，NoError) 构成一个幺半群

data FieldError = NoError | FieldError { getError :: String } deriving (Show)

instance Monoid FieldError where
  mempty = NoError
  FieldError errors `mappend` _ = FieldError errors
  x `mappend` NoError = x
  NoError `mappend` x = x

type ErrorCheckFunc = String -> FieldError

mustHaveMixedLetters :: ErrorCheckFunc
mustHaveMixedLetters str = if all (\checker -> any checker str) [isUpper, isLower, isDigit]
                            then NoError
                            else FieldError $ "password must have uppercase, lowercase letters and digits!"

mustHaveSpecialLetter :: ErrorCheckFunc
mustHaveSpecialLetter str = if any (`elem` specialLetters) str
                              then NoError
                              else FieldError $ "password must have special letters like " ++ specialLetters ++ "!"
  where specialLetters = "_/\\*_=+?"

mustHaveLength :: Int -> ErrorCheckFunc
mustHaveLength len str = if length str >= len
                              then NoError
                              else FieldError $ "must have length " ++ show len ++ "!"

checkPasswordValid :: String -> FieldError
checkPasswordValid str = mconcat $ map ($ str) [mustHaveMixedLetters, mustHaveSpecialLetter, mustHaveLength 16]

-- :m + Data.Monoid
-- 使用现成的 First


checkPasswordValid' :: String -> Maybe String
checkPasswordValid' str =
  getLast . mconcat . map (typeTransformer . ($ str)) $ [mustHaveMixedLetters, mustHaveSpecialLetter, mustHaveLength 16]
  where 
    typeTransformer :: FieldError -> Last String
    typeTransformer NoError = Last Nothing
    typeTransformer (FieldError errorMsg) = Last $ Just errorMsg
  
-- Last 可改为 First，保留第一个还是最后一个错误信息
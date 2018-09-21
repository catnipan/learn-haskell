import qualified Data.Map as Map

-- type String = [Char]
-- 类型别名，让代码更易读

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook = 
  [("betty", "111-222"),
  ("alice", "333-444"),
  ("wendy", "444-555")]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

type AssocList k v = [(k, v)]
-- 带参数的类型别名

type IntMap = Map.Map Int
-- 不完全调用类型构造子 Map 
-- 等价于
-- type IntMap v = Map Int v

-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)

type LockerNo = Int
type Code = String
type TakenError = String
type LockerMap = Map.Map LockerNo (LockerState, Code)

type LookupResult = Either TakenError Code

lockerLookup :: LockerNo -> LockerMap -> LookupResult
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " does not exist"
    Just (state, code) -> if state /= Taken
                            then Right code
                            else Left $ "Locker " ++ show lockerNumber ++ " was already taken"

defaultLocker :: LockerMap
defaultLocker = Map.fromList
  [(12, (Free, "1234")),
   (31, (Free, "3133")),
   (29, (Taken, "0001"))]
  
askForDefaultLocker :: LockerNo -> LookupResult
askForDefaultLocker = flip lockerLookup defaultLocker
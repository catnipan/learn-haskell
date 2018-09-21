class YesNo a where
  yesno :: a -> Bool
-- 我们定义了一个名为 YesNo 的 typeclass
-- 一个 type 想要属于这个 typeclass，那么需要定义一个函数为 yesno
-- 这个函数将此 type 的值映射到 Bool 类型

-- 我们让 Int 这个 type 属于 YesNo typeclass
-- 提供 instance 如下
instance YesNo Int where
  yesno 0 = False
  yesno _ = True

-- 让 [a] 这个 type 属于 YesNo typeclass
-- 提供 instance 如下
instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False
  
yesnoIf :: (YesNo a) => a -> b -> b -> b
yesnoIf val trueResult falseResult =
  if yesno val then trueResult else falseResult  
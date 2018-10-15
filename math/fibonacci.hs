type BinaryOperation = Integer -> Integer -> Integer

seqStartWith :: BinaryOperation -> Integer -> Integer -> [Integer]
seqStartWith op a b = a:(seqStartWith op b (a `op` b))

fibonacci :: [Integer]
fibonacci = seqStartWith (+) 1 1

lucasSeq :: [Integer]
lucasSeq = seqStartWith (+) 1 3

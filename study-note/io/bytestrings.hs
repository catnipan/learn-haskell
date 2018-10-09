import System.Environment
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

-- Bytesstrings 有 strict 和 lazy 两种
-- Data.ByteString
-- 一个 strict bytestring 代表一连串的 bytes
-- Data.ByteString.Lazy
-- 存在 chunks 中，每个 chunk 大小为 64k

-- B.pack :: [GHC.Word.Word8] -> B.ByteString
-- Word8 是介于 0 - 255 之间的整数
-- B.pack 函数把一些数值打包成 ByteString
-- B.pack [99,97,110]
-- "can"

-- B.unpack :: B.ByteString -> [GHC.Word.Word8]
-- B.unpack 把一个 bytestring 变为一个 byte list

-- B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack[46,47,48]]
-- B.fromChunks :: [S.ByteString] -> B.ByteString
-- B.fromChunks 把一串 strict 的 bytestrings 变为一串 lazy 的 bytestring

-- B.toChunks :: B.ByteString -> [S.ByteString]
-- B.toChunks　把 lazy bytestring 变为一串 strict bytestrings

-- B.cons 是 bytestring 版本的 (:)
-- B.cons :: GHC.Word.Word8 -> B.ByteString -> B.ByteString
-- B.cons 85 $ B.pack [80,81,82,84]
-- "UPQRT"
-- 其实是 Chunk "U" (Chunk "PQRT" Empty) 

-- B.cons 是 lazy 的操作，即使 bytestring 的第一个 chunk 不是满的，他也会新增一个 chunk。这也是为什么当你要插入很多 bytes 的时候最好用 strict 版本的 cons，也就是 B.cons'
-- B.cons' 85 $ B.pack [80,81,82,84]
-- Chunk "UPQRT" Empty

main = do
  (fileName1:fileName2:_) <- getArgs
  copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
  contents <- B.readFile source
  B.writeFile dest contents
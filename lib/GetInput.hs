module GetInput
where

import GHC.IO.Encoding (utf8)
import System.IO (Handle, IOMode (ReadMode), hGetContents, hSetEncoding, withFile)

readUtf8Strict :: Handle -> IO String
readUtf8Strict h = do
  hSetEncoding h utf8
  c <- hGetContents h
  last (c ++ "\0") `seq` return c

readFromFile :: FilePath -> IO String
readFromFile filename = withFile filename ReadMode readUtf8Strict

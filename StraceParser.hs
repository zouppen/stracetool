module StraceParser where

import Data.Attoparsec.ByteString
import Data.ByteString (ByteString, pack)
import Data.Functor

data Trace = Trace {
  raw :: ByteString
} deriving (Show)

-- |Parse single trace
trace :: Parser Trace
trace = (Trace . pack) <$> manyTill anyWord8 (word8 10)


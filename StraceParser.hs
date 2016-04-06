{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module StraceParser where

import Data.Attoparsec.Text
import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.ByteString (ByteString, pack)
import Control.Applicative

data Trace = Trace { ts      :: Scientific
                   , command :: Text
                   , args    :: [Arg]
                   , ret     :: Integer
} deriving (Show)

data Arg = EnumArg Text
         | NumericArg Scientific
         | BytesArg ByteString
         deriving (Show)

-- |Parse single trace
trace :: Parser Trace
trace = do
  ts <- scientific
  " "
  command <- takeTill (=='(')
  "("
  args <- arg `sepBy` ", "
  ")"
  skipMany " "
  "= "
  ret <- signed decimal
  endOfLine
  return $ Trace{..}

-- |Parse all argument types, wrapping the result to one of in Args.
arg = NumericArg <$> scientific <|>
      EnumArg <$> enumArg <|>
      BytesArg <$> bytesArg

-- |Parse Enum which is a string without starting quote.
enumArg = takeWhile1 $ notInClass "\",)"

-- |Parse escaped string (in strace's -xx format)
bytesArg = do
  char '"'
  octets <- many ("\\x" >> hexadecimal)
  char '"'
  return $ pack octets

-- |If trace was whole then it ends with this sequence.
traceEnd = do
  ts <- scientific
  " +++ exited with "
  decimal
  " +++\n"
  endOfInput

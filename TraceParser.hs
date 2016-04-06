{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module TraceParser (trace, traceEnd) where

import Data.Attoparsec.Text
import Data.ByteString (pack)
import Control.Applicative

import Trace

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
  errStr <- optional errorField
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

-- |Take possible error message after return code
errorField = " " >> takeTill (=='\n')

-- |If trace was whole then it ends with this sequence.
traceEnd = do
  ts <- scientific
  " +++ exited with "
  decimal
  " +++\n"
  endOfInput

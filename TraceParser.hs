{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module TraceParser (trace, traceEnd) where

import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.Attoparsec.Text
import Data.ByteString (pack)
import Data.Maybe (isNothing)
import Control.Applicative

import Trace

-- |Parse single trace
trace :: Parser Trace
trace = do
  ts <- scientific
  " "
  Call command args <- call
  skipMany " "
  "= "
  ret <- signed decimal
  errStr <- optional errorField
  endOfLine
  return $ Trace{..}

call = do
  command <- enumArg
  "("
  args <- arg `sepBy` ", "
  ")"
  return $ Call command args

-- |Parse all argument types, wrapping the result to one of in Args.
arg = NumericArg <$> hexArg <|>
      NumericArg <$> scientific <|>
      bytesArg <|>
      FieldArg <$> fieldArg <|>
      CallArg <$> call <|>
      EnumArg <$> enumArg

hexArg :: Parser Scientific
hexArg = do
  "0x"
  a <- hexadecimal
  return $ fromIntegral (a :: Integer)

-- |Parse Enum which is a string without starting quote.
enumArg = takeWhile1 $ notInClass "\",{}()"

-- |Parse escaped string (in strace's -xx format)
bytesArg = do
  optional "@"
  char '"'
  octets <- many singleByte
  char '"'
  ellipsis <- optional "..."
  return $ BytesArg (pack octets) (isNothing ellipsis)

-- |Not perfect parser but takes both hex escaped and "easy" characters
-- which may exist in more complex fields.
singleByte = ("\\x" >> hexadecimal) <|>
             (fromIntegral . fromEnum) <$> satisfy (notInClass "\\\"") -- YÄRGH!

fieldArg :: Parser [(Text, Arg)]
fieldArg = do
  "{"
  xs <- fieldPair `sepBy` ", "
  "}"
  return xs
  where fieldPair = do
          a <- takeTill (=='=')
          "="
          b <- arg
          return (a,b)

-- |Take possible error message after return code
errorField = " " >> takeTill (=='\n')

-- |If trace was whole then it ends with this sequence.
traceEnd = do
  ts <- scientific
  " +++ exited with "
  decimal
  " +++\n"
  endOfInput

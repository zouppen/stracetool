module Trace where

import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.ByteString (ByteString)

data Trace = Trace { ts      :: Scientific
                   , command :: Text
                   , args    :: [Arg]
                   , ret     :: Int
                   , errStr  :: Maybe Text
} deriving (Show)

data Arg = EnumArg Text
         | FieldArg [(Text, Arg)]
         | CallArg Call
         | NumericArg Scientific
         | BytesArg ByteString Bool -- Second field is False if truncated by strace
         deriving (Show)

data Call = Call Text [Arg] deriving (Show)

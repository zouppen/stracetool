module Trace where

import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.ByteString (ByteString)

data Trace = Trace { ts      :: Scientific
                   , command :: Text
                   , args    :: [Arg]
                   , ret     :: Int
} deriving (Show)

data Arg = EnumArg Text
         | NumericArg Scientific
         | BytesArg ByteString
         deriving (Show)


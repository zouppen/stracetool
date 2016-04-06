module Main where

import System.IO
import qualified Data.ByteString as B

import StraceParser
import ParserTools

main = do
  hSetBuffering stdin NoBuffering
  parseTracesFromHandle stdin print

-- |Shorthand for parsing given handle
parseTracesFromHandle :: Handle -> (Trace -> IO ()) -> IO ()
parseTracesFromHandle h = parseMany trace (B.hGetSome h 1024)

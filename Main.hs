module Main where

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T

import StraceParser
import ParserTools

main = do
  hSetBuffering stdin LineBuffering
  parseTracesFromHandle stdin print

-- |Shorthand for parsing given handle
parseTracesFromHandle :: Handle -> (Trace -> IO ()) -> IO ()
parseTracesFromHandle h = parseMany trace traceEnd (T.hGetChunk h)

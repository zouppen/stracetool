module Main where

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Trace
import TraceParser
import ParserTools
import FileTracker

main = do
  hSetBuffering stdin LineBuffering
  a <- getProcess
  parseTracesFromHandle stdin a

-- |Shorthand for parsing given handle
parseTracesFromHandle :: Handle -> (Trace -> IO ()) -> IO ()
parseTracesFromHandle h = parseMany trace traceEnd (T.hGetChunk h)

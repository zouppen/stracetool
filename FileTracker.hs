{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module FileTracker where

import Data.IntMap.Strict as IM
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Functor
import Data.Scientific
import Data.IORef

import Trace

type State = IntMap B.ByteString

-- todo dup(2), dup2(2), and dup3(2) support!

process :: State -> Trace -> IO State
process oldMap Trace{..} = do
  case command of
   "open" -> do
     let [BytesArg path,_] = args
     return $ insert ret path oldMap
   "read" -> do
     let [NumericArg fd', BytesArg bytes, _] = args
         Just fd = toBoundedInteger fd'
         path = oldMap ! fd
     B.putStr path
     putStr " < "
     putStrLn $ show bytes
     return oldMap
   "write" -> do
     let [NumericArg fd', BytesArg bytesTried, _] = args
         Just fd = toBoundedInteger fd'
         path = oldMap ! fd
         bytesWritten = B.take ret bytesTried
     B.putStr path
     putStr " > "
     putStrLn $ show bytesWritten
     return oldMap
   _ -> return oldMap -- Unknown syscall

-- |Get state-wrapped Trace processor
getProcess :: IO (Trace -> IO ())
getProcess = do
  ref <- newIORef $ fromList [(0, "/dev/stdin")
                             ,(1, "/dev/stdout")
                             ,(1, "/dev/stderr")
                             ]
  return $ statefulWrap ref process

-- |Wraps IO computation without statekeeping as stateful
statefulWrap :: IORef a -> (a -> b -> IO a) -> b -> IO ()
statefulWrap ref f x = do
  old <- readIORef ref
  f old x >>= writeIORef ref

{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module FileTracker where

import Data.IntMap.Strict as IM
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Functor
import Data.Scientific
import Data.IORef

import Trace

type State = IntMap B.ByteString

process :: State -> Trace -> IO State
process oldMap Trace{..} = do
  case command of
   -- Open file. Store file name of returning descriptor
   "open" -> do
     let BytesArg path = head args
     return $ insert ret path oldMap
   "close" -> do
     let [NumericArg fd'] = args
         Just fd = toBoundedInteger fd'
     return $ if ret == 0
              then delete fd oldMap
              else oldMap -- Closing error in traced program
   -- In many cases we can get target name from sockets, too.
   "connect" -> do
     return $ case args of
               (NumericArg fd':FieldArg [("sa_family",EnumArg "AF_INET"),("sin_port",CallArg (Call "htons" [NumericArg port'])),("sin_addr",CallArg (Call "inet_addr" [BytesArg addr]))]:_) ->
                let Just fd = toBoundedInteger fd'
                    Just port = toBoundedInteger port'
                    str = BC.concat ["IPv4/", addr, ":", BC.pack $ show (port :: Int)]
                in insert fd str oldMap
               _ -> oldMap -- Not recognized (yet)
   -- Duplicate file descriptor.
   "dup" -> genericDup
   "dup2" -> genericDup
   "dup3" -> genericDup
   "read" -> do
     let [NumericArg fd', BytesArg bytes, _] = args
         Just fd = toBoundedInteger fd'
         path = findWithDefault (unknown fd) fd oldMap
     B.putStr path
     putStr " < "
     putStrLn $ show bytes
     return oldMap
   "write" -> do
     let [NumericArg fd', BytesArg bytesTried, _] = args
         Just fd = toBoundedInteger fd'
         path = findWithDefault (unknown fd) fd oldMap
         bytesWritten = B.take ret bytesTried
     B.putStr path
     putStr " > "
     putStrLn $ show bytesWritten
     return oldMap
   _ -> return oldMap -- Unknown syscall
  where genericDup = do
          let NumericArg oldfd' = head args
              Just oldfd = toBoundedInteger oldfd'
          -- Store file name to the new fd, too.
          return $ case IM.lookup oldfd oldMap of
            Nothing -> oldMap -- Perhaps socket dup?
            Just path -> insert ret path oldMap
        unknown fd = "unknown fd #" `B.append` (BC.pack $ show fd)

-- |Get state-wrapped Trace processor
getProcess :: IO (Trace -> IO ())
getProcess = do
  ref <- newIORef $ fromList [(0, "/dev/stdin")
                             ,(1, "/dev/stdout")
                             ,(2, "/dev/stderr")
                             ]
  return $ statefulWrap ref process

-- |Wraps IO computation without statekeeping as stateful
statefulWrap :: IORef a -> (a -> b -> IO a) -> b -> IO ()
statefulWrap ref f x = do
  old <- readIORef ref
  f old x >>= writeIORef ref

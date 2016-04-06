module ParserTools where

import Data.ByteString (ByteString, empty)
import Data.Attoparsec.ByteString
import Control.Monad (forever)

-- |Generic parser which repeteadly reads data from given source,
-- parses it with `parser`, and calls `sink` with parsed data.
parseMany :: Monad m => Parser a -> m ByteString -> (a -> m ()) -> m ()
parseMany parser source sink = forever $ loop empty
  where
    loop input = do
      out <- parseWith source parser input
      case out of
       Done i r  -> do
         sink r
         loop i -- All good, continuing
       Partial _ -> sink $ error "Premature EOF"
       Fail _ _ b -> sink $ error $ "Fail: " ++ b

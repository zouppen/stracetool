module ParserTools (parseMany, endOfInput) where

import Data.Text (Text, empty)
import Data.Attoparsec.Text
import Control.Applicative hiding (empty)

-- |Generic parser which repeteadly reads data from given source,
-- parses it with `itemParser`, and calls `sink` with parsed
-- data. Stops without error only endParser succeeds before end of
-- input. You may supply `endOfInput` if you have no special end
-- sequence.
parseMany :: Monad m => Parser a -> Parser () -> m Text -> (a -> m ()) -> m ()
parseMany itemParser endParser source sink = loop empty
  where
    loop input = do
      out <- parseWith source parser input
      case out of
       Done i (Just r) -> do
         sink r
         loop i -- All good, continuing
       Done _ Nothing -> return () -- EOF. Stop looping.
       Partial _ -> sink $ error "Premature EOF"
       Fail _ a b -> sink $ error $ b ++ " " ++ show a
    -- eofWrap wraps provided parser with Maybe to allow graceful exit
    -- in case of EOF between parser invocations.
    parser = (Just <$> itemParser) <|>
             (endParser >> pure Nothing)

{-# LANGUAGE FlexibleContexts #-}
module Text.Parsec.Extra
  (
    eol
  , digit
  , natural
  , integer
  , caseInsensitiveChar
  , caseInsensitiveString
--  , parseM
  ) where


import           Control.Applicative                      (Applicative,(<$>),(<*>),(<*),(*>),pure)
import           Control.Monad.Error                      (Error,MonadError,throwError) -- ErrorType
import           Control.Monad.Trans.Error                (noMsg,strMsg)
import           Data.Char                                (toLower,toUpper)
import           Data.List                                (foldl')
import           Text.ParserCombinators.Parsec.Prim       (GenParser,(<|>),(<?>),parse)
import           Text.ParserCombinators.Parsec.Combinator (many1,option)
import qualified Text.ParserCombinators.Parsec.Char as Char
import           Text.ParserCombinators.Parsec.Char       (char)


-- | Parse \"end of line\": one of \"\\n\", \"\\r\\n\", or \"\\r\".
eol :: GenParser Char state ()
eol = (char '\n' <|> (char '\r' >> option '\n' (char '\n'))) >> return ()

-- | A decimal digit.
digit :: (Integral a) => GenParser Char state a
digit = fromIntegral . (\ c -> fromEnum c - fromEnum '0') <$> Char.digit

-- | A natural (i.e. non-negative integer) number, in decimal notation.
natural :: (Integral a) => GenParser Char state a
natural = (foldl' (\ a b -> a * 10 + b) 0 <$> many1 digit) <?> "nonnegative decimal integer"

-- | An integer number, in decimal notation (possibly prefixed with \"-\").
integer :: (Integral a) => GenParser Char state a
integer = (option id (char '-' *> pure negate) <*> natural) <?> "decimal integer"

-- | Parse the given character, or the same character in another case
-- (upper or lower).
caseInsensitiveChar :: Char -> GenParser Char state Char
caseInsensitiveChar c = do
  char (toLower c) <|> char (toUpper c)
  return c

-- | Parse the given string, but with any combination of upper and lower case
-- characters.
caseInsensitiveString :: String -> GenParser Char state String
caseInsensitiveString = sequence . map caseInsensitiveChar

-- | Parsing function. Uses the 'MonadError' class to throw a monadic error
-- when parsing fails. (Useful in a stack of monad transformers from the
-- transformers package <http://hackage.haskell.org/package/transformers>.)
--parseM :: (MonadError m,Error (ErrorType m)) => GenParser t () a -> String -> [t] -> m a
--parseM p s = either (throwError . strMsg . show) return . parse p s
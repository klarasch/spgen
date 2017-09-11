module Spgen.Parser where

import Spgen.Definitions

import Data.Either.Combinators
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Error

-- The input document is divided to sections and blocks (in Skeleton, section here is
-- a container and a row, and a block here is a column).

-- | PARSERS ---

-- | end of line
eol :: GenParser Char state ()
eol = (char '\n' <|> (char '\r' >> option '\n' (char '\n'))) >> return ()

-- | whole document ("cuts" document into "raw" unparsed sections)
doc :: Parser [String]
doc = optional (try secDelim) *> many ((try secDelim *> return "") <|> secStr)

-- | a "raw" unparsed (not dividing blocks yet) section
secStr :: Parser String
secStr = do
  c <- anyChar
  s <- manyTill anyChar (eof <|> try (eol *> secDelim))
  return (c : s)

-- | section delimiter - at least three dashes terminated by a newline
secDelim :: Parser ()
secDelim = string ":::" *> many (char ':') *>
  many1 eol *>
  return ()

-- | block
block :: Parser String
block = (try blockDelim *> return "") <|> content

-- | parsing section - getting section parameters and blocks
section :: Parser Section
section = do
  p <- option "default" (char ':' *> (try (choice [
         try (string "default"),
         try (string "dark"),
         try (string "color")
       ] <* (char ':' <?> "a valid section option: default, dark or color"))
       ))
  b <- many block
  return (Section (secParam p) b)

-- | helper function for converting section option strings to values
secParam :: String -> SecParam
secParam "default" = Default
secParam "dark" = Dark
secParam "color" = Color

-- | block delimiter - at least three hyphens terminated by a newline
blockDelim :: Parser ()
blockDelim = string "---" *> many (char '-') *>
  many1 eol *>
  return ()

-- ! content of one block
content :: Parser String
content =
  do c <- anyChar;
     s <- manyTill anyChar (eof <|> try (char '\n' *> blockDelim));
     return (c : s)

-- | PARSING
-- First we "call a parser"
-- to divide the document only to "raw" strings that are not divided to blocks yet,
-- then we parse each section. We do this in two steps as Parsec works linearly
-- and this is a recursive task - and we wont to propagate possible errors well.


parseDoc :: String -> Either ParseError [Section]
parseDoc x = parse doc "" x >>= traverse parseSec

parseSec ::  String -> Either ParseError Section
parseSec = parse section ""

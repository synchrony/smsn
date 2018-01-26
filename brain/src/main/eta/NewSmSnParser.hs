{-
:! ghc -c NewSmSnParser.hs -XFlexibleContexts
:load NewSmSnParser
-}

module NewSmSnParser where


import Data.Text
import Data.List
import Data.Char

import qualified Text.Parsec as Parsec

-- I am the error message infix operator, used later:
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)

data ParserInput = ParserInput { fileName :: String, inputText :: String }
data IndentedLine = IndentedLine { indentChars :: Int, indentedText :: String } deriving Show


expandTabs s = Prelude.concat $ Prelude.map (\c -> if c == '\t' then "    " else [c]) s
stripHead s = if s == [] then [] else if (Data.Char.isSpace $ Prelude.head s) then stripHead(Prelude.tail s) else s
strip s = Prelude.reverse(stripHead(Prelude.reverse(stripHead(s))))
isNonempty line = [] /= (indentedText line)


nonBreakingWhitespace :: Parsec.Parsec String () (String)
nonBreakingWhitespace = Parsec.many $ Parsec.oneOf(" \t")


-- | Note: tabs count as four spaces.
indentation = do
  ws <- nonBreakingWhitespace
  return (Prelude.length $ expandTabs $ ws)

lineText = do
  text <- Parsec.many $ Parsec.noneOf "\n"
  return (NewSmSnParser.strip $ text)

indentedLine = do
    indent <- indentation
    line <- lineText
    return (IndentedLine indent line)

allLines input = Parsec.parse
    (Parsec.sepBy indentedLine (Parsec.char '\n')) (fileName input) (inputText input)

nonemptyLines input = case (allLines input) of
    Left theError  -> Left (show theError)
    Right theLines -> Right ((Data.List.filter isNonempty) theLines)

text = "One\n\
       \  Two\n\
       \  \n\
       \  Three"

{-
try:
  Parsec.parse NewSmSnParser.lines "file.smsn" text

  allLines (ParserInput "file.smsn" text)
  nonemptyLines (ParserInput "file.smsn" text)
-}


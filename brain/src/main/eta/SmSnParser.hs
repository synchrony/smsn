-- :! ghc -c SmSnParser.hs -XFlexibleContexts

module SmSnParser where


import Data.Text
import Data.Char

import qualified Text.Parsec as Parsec

-- I am the error message infix operator, used later:
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)


data Entity = Entity (Maybe String) String deriving Show

data Property = Property String String deriving Show

data Rel = PlainRel String | AsRel String | WithRel String deriving Show

data Line = EntityLine Entity
          | PropertyLine Property
          | RelLine Rel (Maybe Entity)
          | EmptyLine deriving (Show)

data Indented = Indented Int Line deriving Show


expandTabs s = Prelude.concat $ Prelude.map (\c -> if c == '\t' then "    " else [c]) s
stripHead s = if (Data.Char.isSpace $ Prelude.head s) then stripHead(Prelude.tail s) else s
strip s = Prelude.reverse(stripHead(Prelude.reverse(stripHead(s))))
getId s = if s == [] then Nothing else Just s

requiredSpace = Parsec.many1 $ Parsec.oneOf(" \t")
optionalSpace = Parsec.many $ Parsec.oneOf(" \t")

-- | Note: tabs count as four spaces.
indentation = do
  ws <- optionalSpace
  return (Prelude.length $ expandTabs $ ws)

bullet :: Parsec.Parsec String () (Maybe String)
bullet = do
    Parsec.char '['
    optionalSpace
    id <- Parsec.many $ Parsec.oneOf(['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
    optionalSpace
    Parsec.char ']'
    return (getId id)

propertyKey = Parsec.many1 Parsec.letter
roleName = Parsec.many1 Parsec.letter

lineText = do
  text <- Parsec.many $ Parsec.noneOf "\n"
  return (SmSnParser.strip $ text)

entity = do
    id <- bullet
    requiredSpace
    text <- lineText
    return (Entity id text)

entityLine = do
    ent <- entity
    return (EntityLine ent)

propertyLine = do
    Parsec.char '@'
    key <- propertyKey
    requiredSpace
    value <- lineText
    return (PropertyLine (Property key value))

asIn = do
    requiredSpace
    role <- roleName
    return (AsRel role)

asOut = do
    rest <- roleName
    return (PlainRel ("as" ++ rest))

withIn = do
    requiredSpace
    role <- roleName
    return (WithRel role)

withOut = do
    rest <- roleName
    return (PlainRel ("with" ++ rest))

asRel = do
    Parsec.string "as"
    rel <- asIn <|> asOut
    return rel

withRel = do
    Parsec.string "with"
    rel <- withIn <|> withOut
    return rel

plainRel = do
    role <- roleName
    return (PlainRel role)

yesEntity :: Parsec.Parsec String () (Maybe Entity)
yesEntity = do
    ent <- entity
    return (Just ent)

noEntity :: Parsec.Parsec String () (Maybe Entity)
noEntity = do
    optionalSpace
    return Nothing

relLine = do
    rel <- asRel <|> withRel <|> plainRel
    Parsec.char ':'
    optionalSpace
    ent <- yesEntity <|> noEntity
    return (RelLine rel ent)

emptyLine = do
    optionalSpace
    return (EmptyLine)

indentedLine = do
    indent <- indentation
    line <- entityLine <|> propertyLine <|> relLine <|> emptyLine
    return (Indented indent line)

lines = Parsec.sepBy indentedLine (Parsec.char '\n')


text = "[] Hercules\n\
       \  @aka Herakles\n\
       \\n\
       \  with source: [JJsVfKZm09uxCNjz] Wikipedia\n\
       \    as child: \n\
       \      father: [123] Zeus\n\
       \        mother: [] Alcmene"

-- try:    Parsec.parse SmSnParser.lines "file.smsn" text

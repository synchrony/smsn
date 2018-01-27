{-
:! ghc -c SmSnParser.hs -XFlexibleContexts
:load SmSnParser

parse SmSnParser.lines "file.smsn" text
-}

module SmSnParser where


import Data.Char (isSpace)
import Text.Parsec (Parsec, parse, many, many1, oneOf, noneOf, char, string, letter, sepBy)
import Control.Applicative ((<|>))


data Entity = Entity (Maybe String) String deriving Show

data Property = Property String String deriving Show

data Rel = PlainRel String | AsRel String | WithRel String deriving Show

data Line = EntityLine Entity
          | PropertyLine Property
          | RelLine Rel (Maybe Entity)
          | EmptyLine deriving (Show)

data Indented = Indented Int Line deriving Show


expandTabs s = concat $ map (\c -> if c == '\t' then "    " else [c]) s
stripHead s = if (isSpace $ head s) then (stripHead . tail) s else s
strip = reverse . stripHead . reverse . stripHead
getId s = if s == [] then Nothing else Just s

requiredSpace = many1 $ oneOf(" \t")
optionalSpace = many $ oneOf(" \t")

-- | Note: tabs count as four spaces.
indentation = do
  ws <- optionalSpace
  return (length $ expandTabs $ ws)

bullet :: Parsec String () (Maybe String)
bullet = do
    char '['
    optionalSpace
    id <- many $ oneOf(['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
    optionalSpace
    char ']'
    return (getId id)

propertyKey = many1 letter
roleName = many1 letter

lineText = do
  text <- many $ noneOf "\n"
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
    char '@'
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
    string "as"
    rel <- asIn <|> asOut
    return rel

withRel = do
    string "with"
    rel <- withIn <|> withOut
    return rel

plainRel = do
    role <- roleName
    return (PlainRel role)

yesEntity :: Parsec String () (Maybe Entity)
yesEntity = do
    ent <- entity
    return (Just ent)

noEntity :: Parsec String () (Maybe Entity)
noEntity = do
    optionalSpace
    return Nothing

relLine = do
    rel <- asRel <|> withRel <|> plainRel
    char ':'
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

lines = sepBy indentedLine (char '\n')


text = "[] Hercules  \n\
       \  @aka Herakles\n\
       \\n\
       \  with source: [JJsVfKZm09uxCNjz] Wikipedia\n\
       \    \n\
       \    as child: \n\
       \      father: [123] Zeus\n\
       \        mother: [] Alcmene"

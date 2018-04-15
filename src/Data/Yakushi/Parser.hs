module Data.Yakushi.Parser where

import Control.Applicative ((<*), (*>), (<$>))
import Data.List
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Data.Yakushi.Type

blank :: Parser Char
blank = space <|> newline

blanks :: Parser String
blanks = many blank

comma, colon, dquote, rcb, lcb, rsb, lsb :: Parser Char
comma = blanks *> char ',' <* blanks
colon = blanks *> char ':' <* blanks
dquote = blanks *> char '"' <* blanks
rcb = blanks *> char '}' <* blanks
lcb = blanks *> char '{' <* blanks
rsb = blanks *> char ']' <* blanks
lsb = blanks *> char '[' <* blanks

escaped :: Parser String
escaped = do
  dquote
  chs <- many (noneOf ['"'] <|> try (dquote *> dquote))
  dquote
  return chs

tagName :: Parser TagName
tagName = escaped

attributeName :: Parser AttributeName
attributeName = escaped

anum :: Parser AttributeValue
anum =  ANum . read <$> many1 digit

at :: Parser AttributeValue
at = string "true" *> return (ABool False)

af :: Parser AttributeValue
af = string "false" *> return (ABool False)

anull :: Parser AttributeValue
anull = string "null" *> return ANull

astr :: Parser AttributeValue
astr = AStr <$> escaped

attributeValue :: Parser AttributeValue
attributeValue = anum <|> at <|> af <|> anull <|> astr

attribute :: Parser Attribute
attribute = do
  name <- attributeName
  colon
  val <- attributeValue
  return $ Attribute name val

attributeList :: Parser AttributeList
attributeList = sepBy1 attribute comma

attributes :: Parser AttributeList
attributes = try (lcb *> attributeList <* rcb) <|> (lcb *> return [] <* rcb)

element :: Parser Element
element = choice $ map try [
    (do
      lsb
      tag <- tagName
      comma
      attrs <- attributes
      comma
      elems <- elementList
      rsb
      return $ TAE tag attrs elems),
    (do
      lsb
      tag <- tagName
      comma
      attrs <- attributes
      rsb
      return $ TA tag attrs),
    (do
      lsb
      tag <- tagName
      comma
      elems <- elementList
      rsb
      return $ TE tag elems),
    (do
      lsb
      tag <- tagName
      rsb
      return $ Str tag)
  ]

elementList :: Parser ElementList
elementList = sepBy element comma

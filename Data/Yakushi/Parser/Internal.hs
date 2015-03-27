module Data.Yakushi.Parser.Internal
    (
      element
    ) where

import Data.List
import Control.Applicative ((<*), (*>), (<$>))

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Data.Yakushi.Types.Internal

element :: Parser Element
element =
    choice $ map try
        [(do lsb
             tag <- tagName
             comma
             attr <- attributes
             comma
             elem <- elementList
             rsb
             return $ TAE tag attr elem),
        (do lsb
            tag <- tagName
            comma
            attr <- attributes
            rsb
            return $ TA tag attr),
        (do lsb
            tag <- tagName
            comma
            elem <- elementList
            rsb
            return $ TE tag elem),
        (do lsb
            tag <- tagName
            rsb
            return $ T tag),
        (Str <$> tagName)]


elementList :: Parser [Element]
elementList = sepBy element comma

tagName :: Parser TagName
tagName = escaped

attributes :: Parser [Attribute]
attributes = try (lcb *> attributeList <* rcb) <|> lcb *> return [] <* rcb

attributeList :: Parser [Attribute]
attributeList = sepBy1 attribute comma

attribute :: Parser Attribute
attribute = do
    name <- attributeName
    colon
    val <- attributeValue
    return $ (name, val)

attributeName :: Parser AttributeName
attributeName = escaped

attributeValue :: Parser AttributeValue
attributeValue = anum <|> at <|> af <|> anull <|> astr

anum :: Parser AttributeValue
anum = many1 digit >>= return . ANum . read

at :: Parser AttributeValue
at = string "true" >> return (ABool True)

af :: Parser AttributeValue
af = string "false" >> return (ABool False)

anull :: Parser AttributeValue
anull = string "null" >> return ANull

astr :: Parser AttributeValue
astr = AStr <$> escaped

escaped :: Parser String
escaped = do
    dquote
    chs <- many (noneOf ['"'] <|> try (dquote *> dquote))
    dquote
    return chs

comma :: Parser Char
comma = blanks *> char ',' <* blanks

blank :: Parser Char
blank = space <|> newline

blanks :: Parser String
blanks = many blank

colon :: Parser Char
colon = blanks *> char ':' <* blanks

dquote :: Parser Char
dquote = blanks *> char '"' <* blanks

rcb :: Parser Char
rcb = blanks *> char '}' <* blanks

lcb :: Parser Char
lcb = blanks *> char '{' <* blanks

rsb :: Parser Char
rsb = blanks *> char ']' <* blanks

lsb :: Parser Char
lsb = blanks *> char '[' <* blanks

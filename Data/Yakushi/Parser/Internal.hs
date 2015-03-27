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

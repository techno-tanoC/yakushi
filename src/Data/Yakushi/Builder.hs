module Data.Yakushi.Builder where

import Data.Yakushi.Type

toString :: Element -> String
toString = undefined

tagName :: TagName -> String
tagName t = "\"" ++ t ++ "\""

attributeName :: AttributeName -> String
attributeName n = "\"" ++ n ++ "\""

attributeValue :: AttributeValue -> String
attributeValue (ANum i) = show i
attributeValue (ABool True) = "true"
attributeValue (ABool False) = "false"
attributeValue ANull = "null"
attributeValue (AStr str) = "\"str\""

attribute :: Attribute -> String
attribute (name, val) = attributeName name ++ ":" ++ attributeValue val

attributeList :: AttributeList -> String
attributeList [] = ""
attributeList [a] = attribute a
attributeList (a : as) = attribute a ++ "," ++ attributeList as

attributes :: Attributes -> String
attributes as = "{" ++ attributeList as ++ "}"

element :: Element -> String
element (TAE t as es) = "[" ++ tagName t ++ "," ++ attributes as ++ "," ++ elementList es ++ "]"
element (TA t as) = "[" ++ tagName t ++ "," ++ attributes as ++ "]"
element (TE t es) = "[" ++ tagName t ++ "," ++ elementList es ++ "]"
element (Str t) = "[" ++ tagName t ++ "]"

elementList :: ElementList -> String
elementList [] = ""
elementList [e] = element e
elementList (e : es) = element e ++ "," ++ elementList es

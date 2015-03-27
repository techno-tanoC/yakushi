module Data.Yakushi.Types.Internal
    (
      Element(..)
    , Attributes
    , ElementList
    , Attribute(..)
    , AttributeValue(..)
    , TagName
    , AttributeName
    ) where


data Element =
    TAE TagName Attributes ElementList
  | TA TagName Attributes
  | TE TagName ElementList
  | T TagName
  | Str String
    deriving (Eq, Show)

type Attributes = [Attribute]

type ElementList = [Element]

type Attribute = (AttributeName, AttributeValue)

data AttributeValue =
    ANum Int
  | ABool Bool
  | ANull
  | AStr String
    deriving (Eq, Show)

type TagName = String

type AttributeName = String

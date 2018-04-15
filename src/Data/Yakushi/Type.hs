{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Yakushi.Type where

import GHC.Generics
import Data.Aeson
import Data.Text (pack)
import Data.Vector (fromList)

type TagName = String
type AttributeName = String

data AttributeValue =
    ANum Integer
  | ABool Bool
  | ANull
  | AStr String
  deriving (Eq, Show, Generic)

instance ToJSON AttributeValue where
  toJSON (ANum i) =  Number . fromInteger $ i
  toJSON (ABool b) = Bool b
  toJSON ANull =  Null
  toJSON (AStr s) = toJSON s

data Attribute = Attribute AttributeName AttributeValue
  deriving (Eq, Show, Generic)

instance ToJSON Attribute where
  toJSON (Attribute n v) = object [pack n .= v]

type AttributeList = [Attribute]
type Attributes = [Attribute]

data Element =
    TAE TagName Attributes ElementList
  | TA TagName Attributes
  | TE TagName ElementList
  | Str String
  deriving (Eq, Show, Generic)

instance ToJSON Element where
  toJSON (TAE t as es) = Array $ fromList [
      toJSON t
    , toJSON as
    , toJSON es
    ]
  toJSON (TA t as) = Array $ fromList [
      toJSON t
    , toJSON as
    ]
  toJSON (TE t es) = Array $ fromList [
      toJSON t
    , toJSON es
    ]
  toJSON (Str s) = toJSON s

type ElementList = [Element]

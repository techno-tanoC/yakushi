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

data Attribute = Attribute AttributeName AttributeValue
  deriving (Eq, Show, Generic)

type AttributeList = [Attribute]
type Attributes = [Attribute]

data Element =
    TAE TagName Attributes ElementList
  | TA TagName Attributes
  | TE TagName ElementList
  | Str String
  deriving (Eq, Show, Generic)

type ElementList = [Element]

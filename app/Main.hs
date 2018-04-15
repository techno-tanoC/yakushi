module Main where

import Data.Yakushi.Type
import Data.Yakushi.Parser
-- import Data.Yakushi.Builder

as :: Attributes
as = [
    Attribute "a" ANull
  , Attribute "b" (ABool True)
  ]

ta :: Element
ta = TA "tag" as

st :: Element
st = Str "string element"

tae :: Element
tae = TAE "tag" as [ta, st]

main :: IO ()
main = return ()

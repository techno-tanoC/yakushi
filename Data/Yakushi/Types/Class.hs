module Data.Yakushi.Types.Class
    (
      ToJsonML(..)
    , FromJsonML(..)
    where

import Data.Yakushi.Types.Internal

class ToJsonML
    toJsonML :: a -> Element

class FromJsonML
    fromJsonML :: Element -> a

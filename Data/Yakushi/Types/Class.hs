module Data.Yakushi.Types.Class
    (
      ToJsonML(..)
    , FromJsonML(..)
    ) where

import Data.Yakushi.Types.Internal

class ToJsonML a where
    toJsonML :: a -> Element

class FromJsonML a where
    parseJsonML :: Element -> a

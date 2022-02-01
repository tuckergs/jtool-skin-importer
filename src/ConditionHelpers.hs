
module ConditionHelpers where

import Control.Lens
import Data.Monoid
import Text.XML.Light.Types
import XMLStuff

-- This module provides combinators for using the traced comonad for combining conditions together
-- This isn't used anywhere. I thought that it would be useful but it turned out not being useful

hasName :: String -> Element -> Bool
hasName nm e = e^.eName == nm

class IsBool a where
  toBool :: a -> Bool
  fromBool :: Bool -> a

instance IsBool Any where
  toBool = getAny
  fromBool = Any

instance IsBool All where
  toBool = getAll
  fromBool = All


type CondExt m = (m -> Element -> Bool) -> Element -> Bool

makeExt :: IsBool m => (Element -> Bool) -> (m -> Element -> r) -> Element -> r
makeExt p f e = f (fromBool (p e)) e

flatExt :: IsBool m => ((m -> Element -> Bool) -> Element -> Bool) -> Element -> Bool
flatExt ell e = ell (\m _ -> toBool m) e

flatExtAll = flatExt @All
flatExtAny = flatExt @Any

extHasName :: IsBool m => String -> CondExt m
extHasName = makeExt . hasName


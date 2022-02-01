
module XMLStuff where

import Control.Lens
import Text.XML.Light

makePrisms ''Content
makeLensesFor [("elName","eQName"),("elAttribs","eAttrs"),("elContent","eContents"),("elLine","eLine")] ''Element
makeLensesFor [("attrKey","aQKey"),("attrVal","aVal")] ''Attr
makeLensesFor [("cdData","cData")] ''CData

qNme :: Lens' QName String
qNme = lens qName (const unqual)

eName = eQName . qNme
aKey = aQKey . qNme

eContentsEach = eContents . traverse
eElemsEach = eContentsEach . _Elem
eAttrsEach = eAttrs . traverse

txtData :: Traversal' Content String
txtData = _Text . cData

replaceAllChildren :: Element -> [Element] -> Element
replaceAllChildren par newChildren = unode (par^.eName) (par^.eAttrs,newChildren)

modChild :: (Element -> Bool) -> (Element -> Element) -> Element -> Element
modChild p = over (eElemsEach . filtered p)

modChildByName :: String -> (Element -> Element) -> Element -> Element
modChildByName nm f = modChild (anyOf eName (==nm)) f

modAttr :: (Attr -> Bool) -> (Attr -> Attr) -> Element -> Element
modAttr p = over (eAttrsEach . filtered p)

modAttrByName :: String -> (Attr -> Attr) -> Element -> Element
modAttrByName nm = modAttr (\a -> a^.aKey == nm)

eElemsWithNames :: [String] -> Traversal' Element Element
eElemsWithNames ls = eElemsEach . filtered (\e -> e^.eName `elem` ls)


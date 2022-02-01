
module ConfigStuff where

import Control.Lens
import Data.Char
import System.Exit
import Text.XML.Light
import Helpers
import Types
import XMLStuff


parseSkinInfo :: Element -> SingleRes String
parseSkinInfo = foldMapOf (eElemsWithNames ["skinInfo"] . eElemsWithNames ["dir"] . eContentsEach . txtData) Res

expectedNamesForType :: ObjectType -> [String]
expectedNamesForType Apple = ["apple","cherry"]
expectedNamesForType SidePlat = ["sideplat","sidewaysplatform"]
expectedNamesForType ty = [map toLower (show ty)]

parseThing :: ObjectType -> [Element] -> SingleRes String
parseThing ty = 
  let filterTypeNames = filtered (\e -> map toLower (e^.eName) `elem` expectedNamesForType ty)
  in foldMapOf (folded . filterTypeNames . eContentsEach . txtData) Res

-- Expected thing lists have root "sprites" or "objects"
parseThingList :: [String] -> String -> [Element] -> NameList
parseThingList expectedThingListNames preprefix es = NameList $ \ty -> 
  let
    thingListElems = toListOf (folded . eElemsWithNames expectedThingListNames) es
    useDefaults = has (folded . eElemsWithNames ["useDefaults","useDefault"]) thingListElems
    prefixRes = foldMapOf (folded . eElemsWithNames ["prefix"] . eContentsEach . txtData) Res es
    prefixEither = singleRes (Left "No prefix tag") (Left "More than one prefix tag") Right prefixRes
    defEither = fmap (\prfx -> preprefix ++ prfx ++ defaultThingSuffix ty) prefixEither
    thingRes = parseThing ty thingListElems
    thingTagName = concat $ take 1 $ expectedNamesForType ty
    thingEither = singleRes (Left $ "No tag for " ++ thingTagName) (Left $ "More than one tag for " ++ thingTagName) Right thingRes
  in thingEither `altErrors` defEither

parseBGOrRm :: [String] -> String -> [Element] -> Either String String
parseBGOrRm expectedBGRmNames preprefix es =
  let
    bgRmElems = toListOf (folded . eElemsWithNames expectedBGRmNames) es
    useDefaults = has (folded . eElemsWithNames ["useDefaults","useDefault"]) bgRmElems
    prefixRes = foldMapOf (folded . eElemsWithNames ["prefix"] . eContentsEach . txtData) Res es
    prefixEither = singleRes (Left "No prefix tag in info tag") (Left "More than one prefix tag in info tag") Right prefixRes
    defEither = fmap (\prfx -> preprefix ++ prfx) prefixEither
    bgRmRes = foldMapOf (folded . eElemsWithNames ["name"] . eContentsEach . txtData) Res bgRmElems
    bgRmTagName = concat (take 1 expectedBGRmNames)
    bgRmEither = singleRes (Left $ "No name tag for " ++ bgRmTagName) (Left $ "More than one name tag for " ++ bgRmTagName) Right bgRmRes
  in bgRmEither `altErrors` defEither

-- Set info has root "templateInfo" or "outputInfo"
parseSetInfo :: [String] -> Element -> (Either String String, NameList, NameList, Either String String, Either String String)
parseSetInfo expectedNames e =
  let
    setInfoElems = toListOf (eElemsWithNames expectedNames) e
    dirRes = foldMapOf (folded . eElemsWithNames ["dir"] . eContentsEach . txtData) Res setInfoElems
    setInfoTag = concat (take 1 expectedNames)
    dirEither = singleRes (Left $ "No dir tag for " ++ setInfoTag) (Left $ "More than one dir tag for " ++ setInfoTag) Right dirRes
    sprNames = parseThingList ["sprites"] "spr" setInfoElems
    objNames = parseThingList ["objects"] "obj" setInfoElems
    bgName = parseBGOrRm ["background"] "bg" setInfoElems
    rmName = parseBGOrRm ["room","templateRoom"] "rTemplate" setInfoElems
  in (dirEither,sprNames,objNames,bgName,rmName)

parseConfig :: Element -> Either String Config
parseConfig e =
  let
    skinDirRes = parseSkinInfo e
    skinDirEither = singleRes (Left "No skin dir") (Left "More than one skin dir") Right skinDirRes
    (tmpDir,tmpSpr,tmpObj,tmpBG,tmpRm) = parseSetInfo ["templateInfo"] e
    (nwDir,nwSpr,nwObj,nwBG,nwRm) = parseSetInfo ["outputInfo","newInfo"] e
    createConfig sd td nd = Config 
      {
        _templateDir = td ,
        _skinDir = sd ,
        _newDir = nd ,
        _templateSprNames = tmpSpr ,
        _templateObjNames = tmpObj ,
        _newSprNames = nwSpr ,
        _newObjNames = nwObj ,
        _templateBGName = tmpBG ,
        _newBGName = nwBG ,
        _templateRoomName = tmpRm ,
        _newRoomName = nwRm
      }
  in pure createConfig <*> skinDirEither <*> tmpDir <*> nwDir

readConfig :: String -> IO Config
readConfig fileName = do
  fileContents <- readFile fileName
  root <- maybe (die $ "Could not parse " ++ fileName) return (parseXMLDoc fileContents)
  case (parseConfig root) of
    Left msg -> die msg
    Right cfg -> return cfg

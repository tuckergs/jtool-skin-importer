
{-# LANGUAGE LambdaCase #-}

module TestGMStuff2 where

import GMStuff
import Types

suffixes :: ObjectType -> String
suffixes = \case
  Apple -> "Cherry"
  CWater -> "WaterUnrefreshing"
  ty -> show ty


testConfig :: Config
testConfig = Config {
  _templateDir = "collabtest\\Collab Test.gmx",
  _newDir = "test\\output",
  _skinDir = "..\\ctool\\skins\\baka",
  _templateSprNames = NameList $ Right . ("spr29"++) . suffixes,
  _templateObjNames = NameList $ Right . ("obj29"++) . suffixes,
  _newSprNames = NameList $ Right . ("sprBaka"++) . suffixes,
  _newObjNames = NameList $ Right . ("objBaka"++) . suffixes,
  _templateBGName = Right "background7",
  _newBGName = Right "bgBaka",
  _templateRoomName = Right "tTemplateFFH",
  _newRoomName = Right "rTemplateBaka"
}

testGMStuff2 = createNewStuff testConfig


module GMStuff where

import Control.Lens
import Control.Monad
import Data.Maybe
import System.Directory
import System.Exit
import Text.XML.Light
import Helpers
import Types
import XMLStuff

-- XML modification

isFrameZero :: Element -> Bool
isFrameZero e = e^.eName == "frame" && anyOf eAttrsEach (\a -> a^.aKey == "index" && a^.aVal == "0") e

isFrame :: Int -> Element -> Bool
isFrame ind e = e^.eName == "frame" && anyOf eAttrsEach (\a -> a^.aKey == "index" && a^.aVal == show ind) e

numSprFrames :: ObjectType -> Int
numSprFrames = \case
  Apple -> 2
  Save -> 2
  _ -> 1

{-
modSprite :: String -> Element -> Element
modSprite fileName =
  modChildByName "frames"
  $ modChild isFrameZero
  $ set (eContentsEach . txtData) fileName 

modAppleSprite :: String -> String -> Element -> Element
modAppleSprite fileName0 fileName1 =
  modChildByName "frames" $
    modChild (isFrame 0) (set (eContentsEach . txtData) fileName0)
    . modChild (isFrame 1) (set (eContentsEach . txtData) fileName1)
-}

modMultiFrameSprite :: Int -> String -> Element -> Element
modMultiFrameSprite numFrames sprName =
  let
    imageName ind = "images\\" ++ sprName ++ "_" ++ show ind ++ ".png"
    modSingleFrame ind = modChild (isFrame ind) (set (eContentsEach . txtData) (imageName ind))
    modAllFrames = foldr (.) id $ [modSingleFrame ind | ind <- [0..numFrames-1]]
  in modChildByName "frames" modAllFrames 
  

modObject :: String -> Element -> Element
modObject spriteName =
  modChildByName "spriteName"
  $ set (eContentsEach . txtData) spriteName

modBackground :: String -> Element -> Element
modBackground fileName =
  modChildByName "data"
  $ set (eContentsEach . txtData) fileName

isVisibleBG :: Element -> Bool
isVisibleBG e = e^.eName == "background" && anyOf eAttrsEach (\a -> a^.aKey == "visible" && a^.aVal == "-1") e

modTemplateRoom :: String -> Element -> Element
modTemplateRoom bgName =
  modChildByName "backgrounds"
  $ modChild isVisibleBG
  $ modAttrByName "name"
  $ set aVal bgName


-- IO stuff relating to making gm things

modXML :: String -> String -> (Element -> Element) -> IO ()
modXML inFileName outFileName f = do
  inFileContents <- readFile inFileName
  oldRoot <- maybe (die $ "Could not parse " ++ inFileName) return (parseXMLDoc inFileContents)
  let newRoot = f oldRoot
  writeFile outFileName $ showElement newRoot

createObject :: Config -> ObjectType -> String -> String -> String -> String -> IO ()
createObject cfg ty templateSpr newSpr templateObj newObj = do
  let
    templateSprDir = cfg^.templateDir +/+ "sprites"
    newSprDir = cfg^.newDir +/+ "sprites"
    templateObjDir = cfg^.templateDir +/+ "objects"
    newObjDir = cfg^.newDir +/+ "objects"
    skinImagePaths = 
      if (numSprFrames ty == 1)
        then [cfg^.skinDir +/+ toJToolFileName ty]
        else [cfg^.skinDir +/+ (toJToolPrefix ty ++ "_" ++ show ind ++ ".png") | ind <- [0..numSprFrames ty - 1]]
    newImagePaths =
      [newSprDir +/+ "images" +/+ (newSpr ++ "_" ++ show ind ++ ".png") | ind <- [0..numSprFrames ty - 1]]
    templateSprPath = templateSprDir +/+ (templateSpr ++ spriteExt)
    newSprPath = newSprDir +/+ (newSpr ++ spriteExt)
    templateObjPath = templateObjDir +/+ (templateObj ++ objectExt)
    newObjPath = newObjDir +/+ (newObj ++ objectExt)

    makeDir = createDirectoryIfMissing True

  skinImagesExists <- and <$> mapM doesFileExist skinImagePaths
  templateSprExists <- doesFileExist templateSprPath
  templateObjExists <- doesFileExist templateObjPath

  putStrLn $ "Attempting to make object " ++ show ty
  if (skinImagesExists && templateSprExists && templateObjExists) 
    then do
      -- Make directories if missing
      makeDir newSprDir
      makeDir $ newSprDir +/+ "images"
      makeDir newObjDir
      -- Copy new image file(s)
      zipWithM copyFile skinImagePaths newImagePaths
      -- Write sprite xml
      modXML templateSprPath newSprPath 
        (modMultiFrameSprite (numSprFrames ty) newSpr)   
      -- Write object xml
      modXML templateObjPath newObjPath
        (modObject newSpr)
      putStrLn $ "Successfully made object " ++ show ty
    else do
      when (not skinImagesExists) $ putStrLn $ "Could not find " ++ drop 4 (concat [" or " ++ p | p <- skinImagePaths])
      when (not templateSprExists) $ putStrLn $ "Could not find " ++ templateSprPath
      when (not templateObjExists) $ putStrLn $ "Could not find " ++ templateObjPath

createRoom :: Config -> String -> String -> String -> String -> IO ()
createRoom cfg templateBG newBG templateRm newRm = do
  let
    templateBGDir = cfg^.templateDir +/+ "background"
    newBGDir = cfg^.newDir +/+ "background"
    templateRmDir = cfg^.templateDir +/+ "rooms"
    newRmDir = cfg^.newDir +/+ "rooms"

    skinImagePath = cfg^.skinDir +/+ "bg.png"
    newImagePath = newBGDir +/+ "images" +/+ (newBG ++ ".png")
    templateBGPath = templateBGDir +/+ (templateBG ++ backgroundExt)
    newBGPath = newBGDir +/+ (newBG ++ backgroundExt)
    templateRmPath = templateRmDir +/+ (templateRm ++ roomExt)
    newRmPath = newRmDir +/+ (newRm ++ roomExt)

    makeDir = createDirectoryIfMissing True
  
  skinImageExists <- doesFileExist skinImagePath
  templateBGExists <- doesFileExist templateBGPath
  templateRmExists <- doesFileExist templateRmPath

  putStrLn "Attempting to make template room"
  if (skinImageExists && templateBGExists && templateRmExists) 
    then do
      makeDir newBGDir
      makeDir $ newBGDir +/+ "images"
      makeDir newRmDir
      copyFile skinImagePath newImagePath
      modXML templateBGPath newBGPath
        (modBackground ("images\\" ++ newBG ++ ".png"))
      modXML templateRmPath newRmPath
        (modTemplateRoom newBG)
      putStrLn $ "Successfully made template room"
    else do
      when (not skinImageExists) $ putStrLn $ "Could not find " ++ skinImagePath
      when (not templateBGExists) $ putStrLn $ "Could not find " ++ templateBGPath
      when (not templateRmExists) $ putStrLn $ "Could not find " ++ templateRmPath
      

createNewStuff :: Config -> IO ()
createNewStuff cfg = do
  let 
    --flatten = fromMaybe (return ())
    flatten = either putStrLn id
  forM_ allObjectTypes $ \ty -> do
    flatten $ pure (createObject cfg ty) <*> _nameList (_templateSprNames cfg) ty <*> _nameList (_newSprNames cfg) ty <*> _nameList (_templateObjNames cfg) ty <*> _nameList (_newObjNames cfg) ty
  flatten $ pure (createRoom cfg) <*> _templateBGName cfg <*> _newBGName cfg <*> _templateRoomName cfg <*> _newRoomName cfg

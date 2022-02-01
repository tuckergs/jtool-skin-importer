
module TestGMStuff where

import Data.Maybe
import System.Exit
import System.IO
import Text.XML.Light
import GMStuff

--modXML :: String -> String -> (Element -> Element) -> IO ()
--modXML inFileName outFileName f = do
  --inFileContents <- readFile inFileName
  --oldRoot <- maybe (die $ "Could not parse " ++ inFileName) return (parseXMLDoc inFileContents)
  --let newRoot = f oldRoot
  --writeFile outFileName $ showElement newRoot

testGMStuff :: IO ()
testGMStuff = do
  modXML "sprNorthSpikeUp.sprite.gmx" "sprCTCSpikeUp.sprite.gmx" (modMultiFrameSprite 1 "sprCTCSpikeUp")
  modXML "objNorthSpikeUp.object.gmx" "objCTCSpikeUp.object.gmx" (modObject "sprCTCSpikeUp")
  modXML "background12.background.gmx" "bgCTC.background.gmx" (modBackground "images\\bgCTC.png")
  modXML "rTemplateDark.room.gmx" "rTemplateCTC.room.gmx" (modTemplateRoom "bgCTC")

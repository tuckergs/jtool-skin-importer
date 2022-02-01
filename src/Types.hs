
module Types where

import Control.Lens
import Data.Char

data ObjectType = 
  Apple
  | Block 
  | CWater
  | JumpRefresher
  | KillerBlock
  | MiniBlock 
  | MiniDown 
  | MiniLeft 
  | MiniRight 
  | MiniUp 
  | Platform
  | Save 
  | SidePlat 
  | SpikeDown
  | SpikeLeft
  | SpikeRight
  | SpikeUp
  | WalljumpL
  | WalljumpR
  | Warp
  | Water1
  | Water2
  | Water3
  deriving (Enum, Eq, Show)

allObjectTypes :: [ObjectType]
allObjectTypes = enumFromTo Apple Water3

toJToolPrefix :: ObjectType -> String
toJToolPrefix WalljumpL = "walljumpL"
toJToolPrefix WalljumpR = "walljumpR"
toJToolPrefix ty = map toLower (show ty)

toJToolFileName = (++".png") . toJToolPrefix

defaultThingSuffix :: ObjectType -> String
defaultThingSuffix Apple = "Cherry"
defaultThingSuffix CWater = "WaterUnrefreshing"
defaultThingSuffix SidePlat = "SidewaysPlatform"

spriteExt = ".sprite.gmx"
objectExt = ".object.gmx"
backgroundExt = ".background.gmx"
roomExt = ".room.gmx"

newtype NameList = NameList { _nameList :: ObjectType -> Either String String }

instance Show NameList where
  show (NameList fn) = show [(ty,fn ty) | ty <- allObjectTypes]

data Config = Config {
  _templateDir :: String ,
  _skinDir :: String ,
  _newDir :: String ,
  _templateSprNames :: NameList ,
  _templateObjNames :: NameList ,
  _newSprNames :: NameList ,
  _newObjNames :: NameList ,
  _templateBGName :: Either String String ,
  _newBGName :: Either String String ,
  _templateRoomName :: Either String String ,
  _newRoomName :: Either String String 
} deriving Show
makeLenses ''Config

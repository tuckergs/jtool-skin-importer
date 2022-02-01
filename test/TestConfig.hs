
module TestConfig where

import System.IO
import ConfigStuff

testConfig :: IO ()
testConfig = do
  cfg <- readConfig "simpleConfig.xml"
  putStrLn "Config is:"
  print cfg


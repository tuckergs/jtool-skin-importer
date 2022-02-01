module Main where

import Control.Monad
import System.Environment
import System.Exit
import System.IO

import ConfigStuff
import GMStuff
import Types

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    die $ "Usage: ./skinimporter.exe [cfg name]"
  let [cfgFileName] = args

  putStrLn "Reading config..."
  cfg <- readConfig cfgFileName
  putStrLn "Creating objects and template room..."
  createNewStuff cfg
  putStrLn "Done!"

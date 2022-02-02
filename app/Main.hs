module Main where

import Control.Monad
import Data.Functor
import System.Environment
import System.Exit
import System.IO

import ConfigStuff
import GMStuff
import Types

main :: IO ()
main = do
  cfgFileName <- getArgs >>= \case
    [arg] -> return arg
    _ -> die $ "Usage: ./skinimporter.exe [cfg name]"

  putStrLn "Reading config..."
  cfg <- readConfig cfgFileName
  putStrLn "Creating objects and template room..."
  createNewStuff cfg
  putStrLn "Done!"

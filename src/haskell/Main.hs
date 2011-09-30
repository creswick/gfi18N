module Main where

import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist)

import PGF

main :: IO ()
main = do
  pgf <- readPGF pgfFile
  while $ runPrompt pgf
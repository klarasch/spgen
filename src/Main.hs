{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import System.Environment
import System.IO
import System.Directory
import System.Exit

import Spgen.Parser
import Text.Parsec.Error
import Spgen.Writer

import Text.Blaze.Html.Renderer.Pretty

--- ARGUMENT PROCESSING ---
data Args = Args {
              title :: String,          -- page's title
              accent :: String,         -- accent colour
              iFileName :: String,      -- input file name
              oFileName :: String       -- output file name
            }
            deriving (Show, Data, Typeable)

-- | help contents
defaults :: Args
defaults = Args {
  title = ""
        &= typ "PAGETITLE"
        &= help "Page's title",
  accent = "yellow"
         &= typ "ACCENTCOLOR"
         &= help "colour of the \"color\" blocks (accent colour)",
  iFileName = "input.txt"
            &= typ "INPUTFILENAME"
            &= help "Name of the input file (default: input.txt)",
  oFileName = "index.htm"
            &= typ "OUTPUTFILENAME"
            &= help "Name of the output file (default: index.htm)"
}
  &= summary "spgen v0.0: a single page generator for Skeleton CSS boilerplate."
  &= details ["Turns pseudomarkdown into a simple single web page compatible with Skeleton CSS boilerplate.",
              "By default takes input.txt as an input and produces index.htm as an output.",
              "", "See the documentation for details.",
              "", "Klára Scholleová (c) 2017"]


main :: IO ()
main = do
  opts <- cmdArgs defaults
  let ifn = iFileName opts
  iFileExists <- doesFileExist ifn
  if iFileExists
    then do
      md <- readFile ifn
      putStrLn "Input file loaded..."
      let parseResult = parseDoc md
      case parseResult of
        Left msg -> do
        -- parsing doc was unsuccessful
          putStrLn ("Parsing of " ++ ifn ++ "was unsuccessful, parsec yielded following message: \n")
          print msg
          exitFailure
        Right ast -> do
        -- parsing doc was successful, render html
          putStrLn "Parsing successful..."
          let html = makeHtmlMonad (title opts) (accent opts) ast
          let ofn = oFileName opts
          writeFile ofn (renderHtml html)
          --writeFile ofn html
          putStrLn (ofn ++ " created!")
    else do
      -- input file not found
      putStrLn "Can't find the input file. Name it 'input.txt' or specify it with -i."
      exitFailure

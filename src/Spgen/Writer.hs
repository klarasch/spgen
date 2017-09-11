{-# LANGUAGE OverloadedStrings #-}

module Spgen.Writer where

import Spgen.Definitions as D
import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

-- Converting AST from parsing to Blaze Html monad for rendering to HTML later

type PageTitle = String
type AccentColor = String

-- block types - basic design elements, e. g. a third block here
-- is "one-third column" div in Skeleton
data BlockType = Jumbotron
               | Half
               | Third

-- ! makes the block itself out of its content string -
makeBlock :: BlockType -> String -> Html
makeBlock Jumbotron block = H.div ! class_ "jumbotron" $ preEscapedString block
makeBlock Half block = H.div ! class_ "one-half column" $ preEscapedString block
makeBlock Third block = H.div ! class_ "one-third column" $ preEscapedString block

-- ! simple helper function to convert section parameter to class name string
secParam :: SecParam -> AttributeValue
secParam Default = "section"
secParam Dark = "section dark"
secParam Color = "section color"

-- | based on number of blocks in a section, decides on layout and adds the section to monad
makeSection :: Section -> Html
makeSection section =
  H.div ! class_ (secParam (D.param section)) $
    H.div ! class_ "container" $ do
      let bs = blocks section
      case bs of
        -- section with one block results in a jumbotron
        [x] -> makeBlock Jumbotron x
        -- section with three blocks results in three columns
        [x, y, z] -> makeThirdsRow [x, y, z]
        -- otherwise the section has half column layout
        -- (e. g. for five blocks, make it three rows with 2, 2 and 1 block)
        _ -> makeHalves bs

-- | makes section with three columns (blocks)
makeThirdsRow :: [Block] -> Html
makeThirdsRow [x, y, z] =
  H.div ! class_ "row" $ do
    makeBlock Third x
    makeBlock Third y
    makeBlock Third z

makeHalves :: [Block] -> Html
makeHalves [x] =
  H.div ! class_ "row" $ do
    makeBlock Half x
    makeBlock Half "&nbsp;"
makeHalves [x, y] =
  H.div ! class_ "row" $ do
    makeBlock Half x
    makeBlock Half y
makeHalves (x : y : rest) = do
  H.div ! class_ "row" $ do
    makeBlock Half x
    makeBlock Half y
  makeHalves rest

-- | converts AST to HTML monad
astToBody :: [Section] -> Html
astToBody [section] = makeSection section
astToBody (section : rest) = do
  makeSection section
  astToBody rest

-- | creates whole HTML monad
makeHtmlMonad :: PageTitle -> AccentColor -> [Section] -> Html
makeHtmlMonad title accent sections = docTypeHtml $ do
  H.head $ do
    H.title (toHtml title)
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "css/skeleton.css"
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "css/normalize.css"
    H.style $ toHtml (".color {background-color: " ++ accent ++ ";}")
  H.body $
    astToBody sections

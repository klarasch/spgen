module Spgen.Definitions where

data Section = Section {
                 param :: SecParam,
                 blocks :: [Block]
               }
             | EmptySec
             deriving (Show)

data SecParam = Default
              | Dark
              | Color
              deriving Show

type Block = String

--  | todo: more "html abstraction"

--  newtype Block = Block [Par]

--  data Par = Header Int [Inline]
--           | Inline
--
--
--  data Inline = Normal String
--              | Bold String
--              | Italic String
--              deriving Show

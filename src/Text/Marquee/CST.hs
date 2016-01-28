module Text.Marquee.CST (
  Doc
  , DocElement()
  , group
  , blankLine
  , thematicBreak
  , heading
  , headingUnderline
  , indentedLine
  , fenced
  , paragraphLine
  , linkReference
  , blockquoteLine
  , unorderedList
  , orderedList
) where

import Data.Foldable
import Data.String.Marquee

type Doc = [DocElement]

data DocElement = BlankLine
                  -- Leaf blocks
                  | ThematicBreak
                  | Heading Int String
                  | HeadingUnderline Int
                  | IndentedBlock [String]
                  | Fenced String String
                  | ParagraphBlock [String]
                  | LinkReference String String (Maybe String)
                  -- Container blocks
                  | BlockquoteBlock [DocElement]
                  | UListBlock [DocElement]
                  | OListBlock [(Int, DocElement)]
                  deriving (Show)


-- CONSTRUCTION FUNCTIONS

group ::  Doc -> Doc
group []                                               = []
group (IndentedBlock x : IndentedBlock y : xs)        = group $ IndentedBlock (x ++ y) : xs
group (ParagraphBlock x : ParagraphBlock y : xs)      = group $ ParagraphBlock (x ++ y) : xs
group (BlockquoteBlock x : BlockquoteBlock y : xs)    = group $ BlockquoteBlock (x ++ y) : xs
group (BlockquoteBlock x : y@(ParagraphBlock _) : xs) = group $ BlockquoteBlock (x ++ [y]) : xs
group (BlockquoteBlock x : xs)                        = BlockquoteBlock (group x) : group xs
group (UListBlock x : UListBlock y : xs)              = group $ UListBlock (x ++ y) : xs
group (OListBlock x : OListBlock y : xs)              = group $ OListBlock (x ++ y) : xs
group (x:xs)                                          = x : group xs

blankLine :: DocElement
blankLine = BlankLine

thematicBreak :: DocElement
thematicBreak = ThematicBreak

heading :: Int -> String -> DocElement
heading depth str = Heading depth (trim str)

headingUnderline :: Int -> DocElement
headingUnderline = HeadingUnderline

indentedLine :: String -> DocElement
indentedLine = IndentedBlock . (:[])

fenced :: String -> String -> DocElement
fenced = Fenced

paragraphLine :: String -> DocElement
paragraphLine = ParagraphBlock . (:[])

linkReference :: String -> String -> Maybe String -> DocElement
linkReference ref url title = LinkReference (trim ref) (trim url) (trim <$> title)

blockquoteLine :: DocElement -> DocElement
blockquoteLine = BlockquoteBlock . (:[])

unorderedList :: DocElement -> DocElement
unorderedList = UListBlock . (:[])

orderedList :: Int -> DocElement -> DocElement
orderedList x = OListBlock . (:[]) . (,) x

-- HELPER FUNCTIONS



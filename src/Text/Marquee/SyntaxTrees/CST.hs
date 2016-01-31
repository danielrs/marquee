module Text.Marquee.SyntaxTrees.CST where

import Data.Foldable
import Data.String.Marquee

import Data.List.Marquee

type Doc = [DocElement]

data DocElement = BlankLine
                  -- Leaf blocks
                  | ThematicBreak
                  | Heading Int [String]
                  | HeadingUnderline Int
                  | IndentedBlock [String]
                  | Fenced String [String]
                  | ParagraphBlock [String]
                  | LinkReference String String (Maybe String)
                  -- Container blocks
                  | BlockquoteBlock [DocElement]
                  | UListBlock [DocElement]
                  | OListBlock [(Int, DocElement)]
                  deriving (Eq, Show)

-- CONSTRUCTION FUNCTIONS

blankLine :: DocElement
blankLine = BlankLine

thematicBreak :: DocElement
thematicBreak = ThematicBreak

heading :: Int -> String -> DocElement
heading depth str = Heading depth [trim str]

headingUnderline :: Int -> DocElement
headingUnderline = HeadingUnderline

indentedBlock :: String -> DocElement
indentedBlock = IndentedBlock . (:[])

fenced :: String -> [String] -> DocElement
fenced info = Fenced (trim info)

paragraphBlock :: String -> DocElement
paragraphBlock = ParagraphBlock . (:[])

linkReference :: String -> String -> Maybe String -> DocElement
linkReference ref url title = LinkReference (trim ref) (trim url) (trim <$> title)

blockquoteBlock :: DocElement -> DocElement
blockquoteBlock = BlockquoteBlock . (:[])

unorderedList :: DocElement -> DocElement
unorderedList = UListBlock . (:[])

orderedList :: Int -> DocElement -> DocElement
orderedList x = OListBlock . (:[]) . (,) x

-- HELPER FUNCTIONS

clean :: Doc -> Doc
clean = trimDoc . group

group ::  Doc -> Doc
group []                                               = []

group (IndentedBlock x : IndentedBlock y : xs)        = group $ IndentedBlock (x ++ y) : xs

group (ParagraphBlock x : ParagraphBlock y : xs)      = group $ ParagraphBlock (x ++ y) : xs
group (ParagraphBlock x : HeadingUnderline 1 : xs)    = Heading 1 (lines . trim . unlines $ x) : group xs
group (ParagraphBlock x : HeadingUnderline 2 : xs)    = Heading 2 (lines . trim . unlines $ x) : group xs

group (BlockquoteBlock x : BlockquoteBlock y : xs)    = group $ BlockquoteBlock (x ++ y) : xs
group (BlockquoteBlock x : y@(ParagraphBlock _) : xs) = group $ BlockquoteBlock (x ++ [y]) : xs
group (BlockquoteBlock x : xs)                        = BlockquoteBlock (group x) : group xs

group (UListBlock x : UListBlock y : xs)             = group $ UListBlock (x ++ y) : xs
group (UListBlock x : BlankLine : UListBlock y : xs) = group $ UListBlock (x ++ y) : xs
group (OListBlock x : OListBlock y : xs)             = group $ OListBlock (x ++ y) : xs
group (OListBlock x : BlankLine : OListBlock y : xs) = group $ OListBlock (x ++ y) : xs

group (x:xs)                                          = x : group xs

trimDoc :: Doc -> Doc
trimDoc = dropWhileEnd (== BlankLine) . dropWhile (== BlankLine)

trimElement :: DocElement -> DocElement
trimElement (ParagraphBlock xs) = ParagraphBlock . lines . trim . unlines $ xs
trimElement x = x

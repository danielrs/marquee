module Text.Marquee.SyntaxTrees.CST where

import Control.Arrow (second)
import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)

import Data.ByteString.Marquee
import Data.List.Marquee

type Doc = [DocElement]

data DocElement = BlankLine
                  -- Leaf blocks
                  | ThematicBreak
                  | Heading Int [B.ByteString]
                  | HeadingUnderline Int
                  | IndentedBlock [B.ByteString]
                  | Fenced B.ByteString [B.ByteString]
                  | ParagraphBlock [B.ByteString]
                  | LinkReference B.ByteString B.ByteString (Maybe B.ByteString)
                  -- Container blocks
                  | BlockquoteBlock [DocElement]
                  | UListBlock [Doc]
                  | OListBlock [(Int, Doc)]
                  deriving (Eq, Show)

-- CONSTRUCTION FUNCTIONS

blankLine :: DocElement
blankLine = BlankLine

thematicBreak :: DocElement
thematicBreak = ThematicBreak

heading :: Int -> B.ByteString -> DocElement
heading depth str = Heading depth [trim str]

headingUnderline :: Int -> DocElement
headingUnderline = HeadingUnderline

indentedBlock :: B.ByteString -> DocElement
indentedBlock = IndentedBlock . (:[])

fenced :: B.ByteString -> [B.ByteString] -> DocElement
fenced info = Fenced (trim info)

paragraphBlock :: B.ByteString -> DocElement
paragraphBlock = ParagraphBlock . (:[])

linkReference :: B.ByteString -> B.ByteString -> Maybe B.ByteString -> DocElement
linkReference ref url title = LinkReference (trim ref) (trim url) (trim <$> title)

blockquoteBlock :: DocElement -> DocElement
blockquoteBlock = BlockquoteBlock . (:[])

unorderedList :: Doc -> DocElement
unorderedList = UListBlock . (:[])

orderedList :: Int -> Doc -> DocElement
orderedList n = OListBlock . (:[]) . (,) n

-- HELPER FUNCTIONS

clean :: Doc -> Doc
clean = trimDoc . group

group ::  Doc -> Doc
group []                                               = []

group (IndentedBlock x : IndentedBlock y : xs)        = group $ IndentedBlock (x ++ y) : xs

group (ParagraphBlock x : ParagraphBlock y : xs)      = group $ ParagraphBlock (x ++ y) : xs
group (ParagraphBlock x : HeadingUnderline 1 : xs)    = Heading 1 (B.lines . trim . B.unlines $ x) : group xs
group (ParagraphBlock x : HeadingUnderline 2 : xs)    = Heading 2 (B.lines . trim . B.unlines $ x) : group xs

group (BlockquoteBlock x : BlockquoteBlock y : xs)    = group $ BlockquoteBlock (x ++ y) : xs
group (BlockquoteBlock x : y@(ParagraphBlock _) : xs) = group $ BlockquoteBlock (x ++ [y]) : xs
group (BlockquoteBlock x : xs)                        = BlockquoteBlock (clean x) : group xs

group (UListBlock x : UListBlock y : xs)             = group $ UListBlock (x ++ y) : xs
group (UListBlock x : BlankLine : UListBlock y : xs) = group $ UListBlock (x ++ y) : xs
group (UListBlock x : xs)                            = UListBlock (map clean x) : group xs
group (OListBlock x : OListBlock y : xs)             = group $ OListBlock (x ++ y) : xs
group (OListBlock x : BlankLine : OListBlock y : xs) = group $ OListBlock (x ++ y) : xs
group (OListBlock x : xs)                            = OListBlock (map (second clean) x) : group xs

group (x:xs)                                          = x : group xs

trimDoc :: Doc -> Doc
trimDoc = dropWhileEnd (== BlankLine) . dropWhile (== BlankLine)

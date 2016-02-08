module Text.Marquee.SyntaxTrees.CST where

import Control.Arrow (second)
import Data.List (dropWhileEnd)
import Data.Text (Text())
import qualified Data.Text as T

import Data.Text.Marquee
import Data.List.Marquee

type Doc = [DocElement]

data DocElement = BlankLine
                  -- Leaf blocks
                  | ThematicBreak
                  | Heading Int [Text]
                  | HeadingUnderline Int Text
                  | IndentedBlock [Text]
                  | Fenced Text [Text]
                  | HTML Text
                  | ParagraphBlock [Text]
                  | LinkReference Text Text (Maybe Text)
                  -- Container blocks
                  | BlockquoteBlock [DocElement]
                  | UListBlock Char [Doc]
                  | OListBlock Char [(Int, Doc)]
                  deriving (Eq, Show)

-- CONSTRUCTION FUNCTIONS

blankLine :: DocElement
blankLine = BlankLine

thematicBreak :: DocElement
thematicBreak = ThematicBreak

heading :: Int -> Text -> DocElement
heading depth str = Heading depth [trim str]

headingUnderline :: Int -> Text -> DocElement
headingUnderline = HeadingUnderline

indentedBlock :: Text -> DocElement
indentedBlock = IndentedBlock . (:[])

fenced :: Text -> [Text] -> DocElement
fenced info = Fenced (trim info)

html :: Text -> DocElement
html = HTML

paragraphBlock :: Text -> DocElement
paragraphBlock = ParagraphBlock . (:[])

linkReference :: Text -> Text -> Maybe Text -> DocElement
linkReference ref url title = LinkReference (T.toLower . trim $ ref) (trim url) title

blockquoteBlock :: DocElement -> DocElement
blockquoteBlock = BlockquoteBlock . (:[])

unorderedList :: Char -> Doc -> DocElement
unorderedList c = UListBlock c . (:[])

orderedList :: Char -> Int -> Doc -> DocElement
orderedList c n = OListBlock c . (:[]) . (,) n

-- HELPER FUNCTIONS

clean :: Doc -> Doc
clean = trimDoc . group

trimDoc :: Doc -> Doc
trimDoc = dropWhileEnd (== BlankLine) . dropWhile (== BlankLine)

trim' :: [Text] -> [Text]
trim' = T.lines . trim . T.unlines

-- grouping

group ::  Doc -> Doc
group []                                                   = []

group (HeadingUnderline 2 x : xs) | T.length x >= 3   = ThematicBreak : group xs
group (HeadingUnderline _ x : xs)                     = group $ ParagraphBlock [x] : xs

group xs@(IndentedBlock _ : _)                        = groupIndented xs

group (ParagraphBlock x : HeadingUnderline 1 _ : xs)  = Heading 1 (trim' x) : group xs
group (ParagraphBlock x : HeadingUnderline 2 _ : xs)  = Heading 2 (trim' x) : group xs
group (ParagraphBlock x : IndentedBlock y : xs)       = group $ ParagraphBlock (x ++ y) : xs
group (ParagraphBlock x : ParagraphBlock y : xs)      = group $ ParagraphBlock (x ++ y) : xs

group (BlockquoteBlock x : BlockquoteBlock y : xs)    = group $ BlockquoteBlock (x ++ y) : xs
group (BlockquoteBlock x : y@(ParagraphBlock _) : xs) = group $ BlockquoteBlock (x ++ [y]) : xs
group (BlockquoteBlock x : xs)                        = BlockquoteBlock (clean x) : group xs

group (UListBlock a x : UListBlock b y : xs)
  | a == b                                               = group $ UListBlock a (x ++ y) : xs
group (UListBlock a x : BlankLine : UListBlock b y : xs)
  | a == b                                               = group $ UListBlock a (x ++ y) : xs
group (UListBlock a x : xs)                              = UListBlock a (map clean x) : group xs

group (OListBlock a x : OListBlock b y : xs)
  | a == b                                               = group $ OListBlock a (x ++ y) : xs
group (OListBlock a x : BlankLine : OListBlock b y : xs)
  | a == b                                               = group $ OListBlock a (x ++ y) : xs
group (OListBlock a x : xs)                                = OListBlock a (map (second clean) x) : group xs

group (x:xs)                                          = x : group xs

groupIndented :: Doc -> Doc
groupIndented xs = go 0 [] xs
  where go blanks xs (BlankLine : zs) = go (blanks + 1) xs zs
        go blanks xs (IndentedBlock ys : zs) = go 0 (xs ++ replicate blanks T.empty ++ ys) zs
        go blanks xs zs = (IndentedBlock xs) : group (replicate blanks BlankLine ++ zs)

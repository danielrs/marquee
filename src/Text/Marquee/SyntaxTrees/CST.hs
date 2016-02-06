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
                  | ParagraphBlock [Text]
                  | LinkReference Text Text (Maybe Text)
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

heading :: Int -> Text -> DocElement
heading depth str = Heading depth [trim str]

headingUnderline :: Int -> Text -> DocElement
headingUnderline = HeadingUnderline

indentedBlock :: Text -> DocElement
indentedBlock = IndentedBlock . (:[])

fenced :: Text -> [Text] -> DocElement
fenced info = Fenced (trim info)

paragraphBlock :: Text -> DocElement
paragraphBlock = ParagraphBlock . (:[])

linkReference :: Text -> Text -> Maybe Text -> DocElement
linkReference ref url title = LinkReference (T.toLower . trim $ ref) (trim url) (trim <$> title)

blockquoteBlock :: DocElement -> DocElement
blockquoteBlock = BlockquoteBlock . (:[])

unorderedList :: Doc -> DocElement
unorderedList = UListBlock . (:[])

orderedList :: Int -> Doc -> DocElement
orderedList n = OListBlock . (:[]) . (,) n

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

group (UListBlock x : UListBlock y : xs)              = group $ UListBlock (x ++ y) : xs
group (UListBlock x : BlankLine : UListBlock y : xs)  = group $ UListBlock (x ++ y) : xs
group (UListBlock x : xs)                             = UListBlock (map clean x) : group xs

group (OListBlock x : OListBlock y : xs)              = group $ OListBlock (x ++ y) : xs
group (OListBlock x : BlankLine : OListBlock y : xs)  = group $ OListBlock (x ++ y) : xs
group (OListBlock x : xs)                             = OListBlock (map (second clean) x) : group xs

group (x:xs)                                          = x : group xs

groupIndented :: Doc -> Doc
groupIndented xs = go 0 [] xs
  where go blanks xs (BlankLine : zs) = go (blanks + 1) xs zs
        go blanks xs (IndentedBlock ys : zs) = go 0 (xs ++ replicate blanks T.empty ++ ys) zs
        go blanks xs zs = (IndentedBlock xs) : group (replicate blanks BlankLine ++ zs)

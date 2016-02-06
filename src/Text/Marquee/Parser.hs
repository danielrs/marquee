{-# LANGUAGE OverloadedStrings #-}

module Text.Marquee.Parser (renderCST, renderAST) where

import Control.Arrow (second)
import Control.Monad.State(StateT(..))

import Data.Text (Text())
import qualified Data.Text as T (intercalate)

import Data.Attoparsec.Text as Atto
import Data.Attoparsec.Combinator

import Text.Marquee.Parser.Block
import Text.Marquee.Parser.Inline
import qualified Text.Marquee.SyntaxTrees.CST as C
import qualified Text.Marquee.SyntaxTrees.AST as A

renderCST :: Text -> C.Doc
renderCST input = case parse parseBlocks input of
                    Left err -> []
                    Right val -> C.clean . fst $ val
  where parse p = parseOnly (runStateT p [])

renderAST :: Text -> A.Markdown
renderAST = uncurry fromDoc . A.stripLinkReferences . renderCST

fromDoc :: A.LinkMap -> C.Doc -> A.Markdown
fromDoc linkMap = map (fromDocElement linkMap)

fromDocElement :: A.LinkMap -> C.DocElement -> A.MarkdownElement
fromDocElement linkMap C.BlankLine              = A.BlankLine
fromDocElement linkMap C.ThematicBreak          = A.ThematicBreak
fromDocElement linkMap (C.Heading n xs)         = A.Heading n (parseInline linkMap $ T.intercalate "\n" xs)
fromDocElement linkMap (C.HeadingUnderline _ _) = A.ThematicBreak
fromDocElement linkMap (C.IndentedBlock xs)     = A.Indented (T.intercalate "\n" xs)
fromDocElement linkMap (C.Fenced info xs)       = A.Fenced info (T.intercalate "\n" xs)
fromDocElement linkMap (C.ParagraphBlock xs)    = A.Paragraph (parseInline linkMap $ T.intercalate "\n" xs)
fromDocElement linkMap (C.LinkReference _ _ _)  = A.BlankLine
fromDocElement linkMap (C.BlockquoteBlock xs)   = A.Blockquote $ map (fromDocElement linkMap) xs
fromDocElement linkMap (C.UListBlock xs)        = A.UnorderedList $ map (fromDoc linkMap) xs
fromDocElement linkMap (C.OListBlock xs)        = A.OrderedList $ map (second $ fromDoc linkMap) xs

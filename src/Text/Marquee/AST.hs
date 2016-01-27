module Text.Marquee.AST where

import Text.Marquee.CST as CST
import qualified Data.Map as M

-- type Reference = String
-- type URL = String
-- type Link = (URL, Maybe String)

-- data Markdown = Markdown (M.Map Reference Link) MarkdownDoc

-- data MarkdownDoc = Empty
--                 | BlankLine
--                 | Cons Doc Doc
--                 | Char Char
--                 | Text String
--                 | ThematicBreak
--                 | Heading Int Doc
--                 | Indented String
--                 | Fenced String String
--                 | Paragraph Doc
--                 | LinkReference Reference
--                 | Blockquote
--                 | Codespan String
--                 | Italic Doc
--                 | Bold Doc

-- fromCST :: CST.Doc -> Markdown
-- fromCST CST.BlankLine = BlankLine
-- fromCST CST.Cons x y) =

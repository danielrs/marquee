module Text.Marquee.CST where

import Data.Foldable

import Data.String.Marquee

data Doc =  Empty
            | BlankLine
            | Char Char
            | Text String
            -- Leaf blocks
            | ThematicBreak
            | Heading Int Doc
            | IndentedLine String
            | Fenced String String
            | ParagraphLine Doc
            | LinkReference String String (Maybe String)
            -- Container blocks
            | BlockquoteLine Doc
            -- Inline
            | Codespan String
            | Italic Doc
            | Bold Doc
            -- Other
            | Cons Doc Doc
            deriving (Show)

infixr 5 <#>
(<#>) :: Doc -> Doc -> Doc
(<#>) = cons

cons :: Doc -> Doc -> Doc
cons a Empty = a
cons Empty b = b
cons (Char a) (Char b) = Text [a,b]
cons (Char a) (Text bs) = Text $ a:bs
cons (Char a) (Cons (Char b) cs) = Cons (Text [a, b]) cs
cons (Char a) (Cons (Text bs) cs) = Cons (Text $ a:bs) cs
cons a b = Cons a b

linkReference :: String -> String -> Maybe String -> Doc
linkReference ref url title = LinkReference (trim ref) (trim url) (trim <$> title)

links :: Doc -> Doc
links x@(LinkReference _ _ _) = x
links (Cons a b) = cons (links a) (links b)
links _ = Empty

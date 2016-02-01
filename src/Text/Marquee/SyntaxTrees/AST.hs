module Text.Marquee.SyntaxTrees.AST where

import Text.Marquee.SyntaxTrees.CST as C
import qualified Data.Map as M

import Data.String.Marquee

type Markdown = [MarkdownElement]

data MarkdownElement  = BlankLine
                        | ThematicBreak
                        | Heading Int MarkdownInline
                        | Indented [String]
                        | Fenced String [String]
                        | Paragraph MarkdownInline
                        | Blockquote [MarkdownElement]
                        | UnorderedList [Markdown]
                        | OrderedList [(Int, Markdown)]
                        deriving (Eq, Show)

data MarkdownInline = NoInline
                      | LineBreak
                      | HardLineBreak
                      | Text [Char]
                      -- Special
                      | Codespan String
                      | Bold MarkdownInline
                      | Italic MarkdownInline
                      | Link MarkdownInline String (Maybe String)
                      | Image MarkdownInline String (Maybe String)
                      -- Cons
                      | Cons MarkdownInline MarkdownInline
                      deriving (Eq, Show)

-- instance Show MarkdownInline where
--   show NoInline = ""
--   show LineEnding = "\n"
--   show (Text xs) = xs
--   show (Codespan xs) = "`" ++ xs ++ "`"
--   show (Bold xs) = "**" ++ show xs ++ "**"
--   show (Italic xs) = "*" ++ show xs ++ "*"
--   show (Link xs url mtitle) = "[" ++ show xs ++ "]" ++ "(" ++ url ++ ")"
--   show (Cons x y) = show x ++ show y

cons :: MarkdownInline -> MarkdownInline -> MarkdownInline
cons x NoInline                    = x
cons NoInline y                    = y
cons (Text xs) (Text ys)           = Text (xs ++ ys)
cons (Text xs) (Cons (Text ys) zs) = cons (Text $ xs ++ ys) zs
cons x y                           = Cons x y

infixr 5 <#>
(<#>) :: MarkdownInline -> MarkdownInline -> MarkdownInline
(<#>) = cons

consLines :: MarkdownInline -> MarkdownInline -> MarkdownInline
consLines x NoInline = x
consLines NoInline y = y
consLines a b        = cons a (cons LineBreak b)

infixr 5 </>
(</>) :: MarkdownInline -> MarkdownInline -> MarkdownInline
(</>) = consLines

noInline :: MarkdownInline
noInline = NoInline

lineBreak :: MarkdownInline
lineBreak = LineBreak

hardLineBreak :: MarkdownInline
hardLineBreak = HardLineBreak

text :: String -> MarkdownInline
text = Text

codespan :: String -> MarkdownInline
codespan = Codespan . trim

bold :: MarkdownInline -> MarkdownInline
bold = Bold

italic :: MarkdownInline -> MarkdownInline
italic = Italic

link :: MarkdownInline -> String -> Maybe String -> MarkdownInline
link = Link

image :: MarkdownInline -> String -> Maybe String -> MarkdownInline
image = Image

containsLink :: MarkdownInline -> Bool
containsLink (Link _ _ _) = True
containsLink (Cons x y) = containsLink x || containsLink y
containsLink _ = False

plain :: MarkdownInline -> String
plain (Text x)     = x
plain (Codespan x) = x
plain (Bold x)     = plain x
plain (Italic x)   = plain x
plain (Link x _ _) = plain x
plain (Cons x y)   = plain x ++ plain y
plain _            = []

-- CST to AST

type Link = (String, Maybe String)
type LinkMap = M.Map String Link

insertLink :: String -> (String, Maybe String) -> LinkMap -> LinkMap
insertLink = M.insertWith (flip const)

stripLinkReferences :: C.Doc -> (LinkMap, C.Doc)
stripLinkReferences = foldr f (M.empty, [])
  where f (LinkReference ref url mtitle) (map, xs) = (insertLink ref (url, mtitle) map, xs)
        f x (map, xs)                       = (map, x : xs)

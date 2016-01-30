module Text.Marquee.AST where

import Text.Marquee.CST as C
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
                        | UnorderedList [MarkdownElement]
                        | OrderedList [(Int, MarkdownElement)]
                        deriving (Eq, Show)

data MarkdownInline = NoInline
                      | LineEnding
                      | Text [Char]
                      -- Special
                      | Codespan String
                      | Bold MarkdownInline
                      | Italic MarkdownInline
                      | Link MarkdownInline String (Maybe String)
                      -- Cons
                      | Cons MarkdownInline MarkdownInline
                      deriving (Eq, Show)

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
consLines a b        = cons a (cons LineEnding b)

infixr 5 </>
(</>) :: MarkdownInline -> MarkdownInline -> MarkdownInline
(</>) = consLines

noInline :: MarkdownInline
noInline = NoInline

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

containsLink :: MarkdownInline -> Bool
containsLink (Link _ _ _) = True
containsLink (Cons x y) = containsLink x || containsLink y
containsLink _ = False

-- CST to AST

type LinkMap = M.Map String (String, Maybe String)

insertLink :: String -> (String, Maybe String) -> LinkMap -> LinkMap
insertLink = M.insertWith (flip const)

stripLinkReferences :: C.Doc -> (LinkMap, C.Doc)
stripLinkReferences = foldr f (M.empty, [])
  where f (LinkReference ref url mtitle) (map, xs) = (insertLink ref (url, mtitle) map, xs)
        f x (map, xs)                       = (map, x : xs)

module Text.Marquee.SyntaxTrees.AST where

import Data.Text (Text(), append, empty)
import qualified Data.Map as M

import Data.Text.Marquee
import Text.Marquee.SyntaxTrees.CST as C

type Markdown = [MarkdownElement]

data MarkdownElement  = BlankLine
                        | ThematicBreak
                        | Heading Int MarkdownInline
                        | Indented Text
                        | Fenced Text Text
                        | HTML Text
                        | Paragraph MarkdownInline
                        | Blockquote [MarkdownElement]
                        | UnorderedList [Markdown]
                        | OrderedList [(Int, Markdown)]
                        deriving (Eq, Show)

data MarkdownInline = NoInline
                      | LineBreak
                      | HardLineBreak
                      | Text Text
                      -- Special
                      | Codespan Text
                      | Bold MarkdownInline
                      | Italic MarkdownInline
                      | Link MarkdownInline Text (Maybe Text)
                      | Image MarkdownInline Text (Maybe Text)
                      | HTMLText Text
                      -- Cons
                      | Cons MarkdownInline MarkdownInline
                      deriving (Eq, Show)

-- -- instance Show MarkdownInline where
-- --   show NoInline = ""
-- --   show LineEnding = "\n"
-- --   show (Text xs) = xs
-- --   show (Codespan xs) = "`" ++ xs ++ "`"
-- --   show (Bold xs) = "**" ++ show xs ++ "**"
-- --   show (Italic xs) = "*" ++ show xs ++ "*"
-- --   show (Link xs url mtitle) = "[" ++ show xs ++ "]" ++ "(" ++ url ++ ")"
-- --   show (Cons x y) = show x ++ show y

cons :: MarkdownInline -> MarkdownInline -> MarkdownInline
cons x NoInline                    = x
cons NoInline y                    = y
cons (Text xs) (Text ys)           = Text (xs `append` ys)
cons (Text xs) (Cons (Text ys) zs) = cons (Text $ xs `append` ys) zs
cons x y                           = Cons x y

infixr 5 <#>
(<#>) :: MarkdownInline -> MarkdownInline -> MarkdownInline
(<#>) = cons

noInline :: MarkdownInline
noInline = NoInline

lineBreak :: MarkdownInline
lineBreak = LineBreak

softLineBreak :: MarkdownInline
softLineBreak = LineBreak

hardLineBreak :: MarkdownInline
hardLineBreak = HardLineBreak

text :: Text -> MarkdownInline
text = Text

codespan :: Text -> MarkdownInline
codespan = Codespan . trim

em :: MarkdownInline -> MarkdownInline
em (Italic xs) = Bold xs
em xs = Italic xs

link :: MarkdownInline -> Text -> Maybe Text -> MarkdownInline
link = Link

image :: MarkdownInline -> Text -> Maybe Text -> MarkdownInline
image = Image

htmlText :: Text -> MarkdownInline
htmlText = HTMLText

containsLink :: MarkdownInline -> Bool
containsLink (Link _ _ _) = True
containsLink (Cons x y) = containsLink x || containsLink y
containsLink _ = False

plain :: MarkdownInline -> Text
plain (Text x)     = x
plain (Codespan x) = x
plain (Bold x)     = plain x
plain (Italic x)   = plain x
plain (Link x _ _) = plain x
plain (Cons x y)   = plain x `append` plain y
plain _            = empty

-- -- CST to AST

type Link = (Text, Maybe Text)
type LinkMap = M.Map Text Link

stripLinkReferences :: C.Doc -> (LinkMap, C.Doc)
stripLinkReferences = foldr f (M.empty, [])
  where f (LinkReference ref url mtitle) (map, xs) =
          (M.insert ref (url, mtitle) map, xs)
        f x (map, xs) =
          (map, x : xs)

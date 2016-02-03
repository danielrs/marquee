module Text.Marquee.SyntaxTrees.AST where

import Data.ByteString (ByteString(), append, empty)
import qualified Data.Map as M

import Data.ByteString.Marquee
import Text.Marquee.SyntaxTrees.CST as C

type Markdown = [MarkdownElement]

data MarkdownElement  = BlankLine
                        | ThematicBreak
                        | Heading Int MarkdownInline
                        | Indented [ByteString]
                        | Fenced ByteString [ByteString]
                        | Paragraph MarkdownInline
                        | Blockquote [MarkdownElement]
                        | UnorderedList [Markdown]
                        | OrderedList [(Int, Markdown)]
                        deriving (Eq, Show)

data MarkdownInline = NoInline
                      | LineBreak
                      | HardLineBreak
                      | Text ByteString
                      -- Special
                      | Codespan ByteString
                      | Bold MarkdownInline
                      | Italic MarkdownInline
                      | Link MarkdownInline ByteString (Maybe ByteString)
                      | Image MarkdownInline ByteString (Maybe ByteString)
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

text :: ByteString -> MarkdownInline
text = Text

codespan :: ByteString -> MarkdownInline
codespan = Codespan . trim

em :: MarkdownInline -> MarkdownInline
em (Italic xs) = Bold xs
em xs = Italic xs

link :: MarkdownInline -> ByteString -> Maybe ByteString -> MarkdownInline
link = Link

image :: MarkdownInline -> ByteString -> Maybe ByteString -> MarkdownInline
image = Image

containsLink :: MarkdownInline -> Bool
containsLink (Link _ _ _) = True
containsLink (Cons x y) = containsLink x || containsLink y
containsLink _ = False

plain :: MarkdownInline -> ByteString
plain (Text x)     = x
plain (Codespan x) = x
plain (Bold x)     = plain x
plain (Italic x)   = plain x
plain (Link x _ _) = plain x
plain (Cons x y)   = plain x `append` plain y
plain _            = empty

-- -- CST to AST

type Link = (ByteString, Maybe ByteString)
type LinkMap = M.Map ByteString Link

insertLink :: ByteString -> (ByteString, Maybe ByteString) -> LinkMap -> LinkMap
insertLink = M.insertWith (flip const)

stripLinkReferences :: C.Doc -> (LinkMap, C.Doc)
stripLinkReferences = foldr f (M.empty, [])
  where f (LinkReference ref url mtitle) (map, xs) =
          (insertLink ref (url, mtitle) map, xs)
        f x (map, xs) =
          (map, x : xs)

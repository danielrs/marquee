{-# LANGUAGE OverloadedStrings #-}

module Text.Marquee.Writers.HTML (renderHtml, writeHtml, writeHtmlDocument) where

-- Control and Data imports
import Control.Monad (forM_)
import qualified Data.Text as T
import Data.List (intercalate)

-- Blaze imports
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A hiding (id)
import qualified Text.Blaze.Html.Renderer.String as H

-- Own imports
import Data.List.Marquee
import Text.Marquee.SyntaxTrees.AST hiding (codespan, em)

renderHtml :: Html -> String
renderHtml = H.renderHtml

writeHtml :: Markdown -> Html
writeHtml = writeHtml'

writeHtmlDocument :: String -> Maybe String -> Markdown -> Html
writeHtmlDocument title Nothing md = docTypeHtml $ do
  H.head . H.title . toHtml $ title
  H.body . writeHtml' $ md
writeHtmlDocument title (Just cssFile) md = docTypeHtml $ do
  H.head $ do
    H.title . toHtml $ title
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! (A.href . toValue $ cssFile)
  H.body . writeHtml' $ md

writeHtml' :: Markdown -> Html
writeHtml' []     = return ()
writeHtml' (x:xs) = writeElement x >> writeHtml' xs

writeNestedHtml :: Markdown -> Html
writeNestedHtml (x : []) = writeSingleElement x
writeNestedHtml xs       = writeHtml' xs

writeElement :: MarkdownElement -> Html
writeElement (ThematicBreak)   = hr
writeElement (Heading n x)     = (lookupOr h4 n headings) $ writeInline x
writeElement (Indented str)    = codeblock "" str
writeElement (Fenced info str) = codeblock info str
writeElement (HTML x)          = preEscapedText x
writeElement (Paragraph x)     = p $ writeInline x
writeElement (Blockquote x)    = H.blockquote $ writeHtml' x
writeElement (UnorderedList x) = ul $ forM_ x (li . writeNestedHtml)
writeElement (OrderedList x)
  | first /= 1                 = ol ! start (toValue first)  $ forM_ x (li . writeNestedHtml . snd)
  | otherwise                  = ol $ forM_ x (li . writeNestedHtml . snd)
  where first = fst . Prelude.head $ x
writeElement _                 = return ()

writeSingleElement :: MarkdownElement -> Html
writeSingleElement (Paragraph x) = writeInline x
writeSingleElement x = writeElement x

writeInline :: MarkdownInline -> Html
writeInline (HardLineBreak)           = br
writeInline (LineBreak)               = toHtml (" " :: String)
writeInline (Text x)                  = H.text x
writeInline (Codespan x)              = code . H.text $ x
writeInline (Bold x)                  = strong $ writeInline x
writeInline (Italic x)                = em $ writeInline x
writeInline (Link x url Nothing)      = (a $ writeInline x) ! href (textValue url)
writeInline (Link x url (Just title)) = (a $ writeInline x) ! href (textValue url) ! A.title (textValue title)
writeInline (Image x dest mtitle)     = img ! alt (textValue . plain $ x) ! src (textValue dest)
writeInline (HTMLText x)              = preEscapedText x
writeInline (Cons x y)                = writeInline x >> writeInline y
writeInline _                         = return ()

codeblock :: T.Text -> T.Text -> Html
codeblock info xs = codeblock' (T.unpack info) (T.unpack $ xs `T.append` "\n")
  where codeblock' :: String -> String -> Html
        codeblock' []   = pre . code . toHtml
        codeblock' info = pre . flip (!) (class_ $ toValue $ "language-" ++ info) . code . toHtml

headings :: [(Int, Html -> Html)]
headings = [(1, h1), (2, h2), (3, h3), (4, h4), (5, h5), (6, h6)]

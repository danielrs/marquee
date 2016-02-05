{-# LANGUAGE OverloadedStrings #-}

module Text.Marquee.Writers.HTMLBlaze (renderHtml, writeHtml, writeHtmlDocument) where

-- Control and Data imports
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as B
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

writeHtml' []     = return ()
writeHtml' (x:xs) = writeElement x >> writeHtml' xs

writeElement :: MarkdownElement -> Html
writeElement (ThematicBreak)   = hr
writeElement (Heading n x)     = (lookupOr h4 n headings) $ writeInline x
writeElement (Indented str)    = codeblock "" str
writeElement (Fenced info str) = codeblock info str
writeElement (Paragraph x)     = p $ writeInline x
writeElement (Blockquote x)    = H.blockquote $ writeHtml' x
writeElement (UnorderedList x) = ul $ forM_ x (li . writeHtml')
writeElement (OrderedList x)   = ol $ forM_ x (li . writeHtml' . snd)
writeElement _                 = return ()

writeInline :: MarkdownInline -> Html
writeInline (HardLineBreak)       = br
writeInline (LineBreak)           = toHtml (" " :: String)
writeInline (Text x)              = toHtml' x
writeInline (Codespan x)          = code . toHtml' $ x
writeInline (Bold x)              = strong $ writeInline x
writeInline (Italic x)            = em $ writeInline x
writeInline (Link x dest mtitle)  =
  let url   = toValue' dest
      title = toValue' $ maybe dest id mtitle
  in  (a $ writeInline x) ! href url ! alt title
writeInline (Image x dest mtitle) = img ! alt (toValue' . plain $ x) ! src (toValue' dest)
writeInline (Cons x y)            = writeInline x >> writeInline y
writeInline _                     = return ()

codeblock :: B.ByteString -> [B.ByteString] -> Html
codeblock info xs = codeblock' (B.unpack info) (B.unpack $ B.intercalate "\n" xs)
  where codeblock' :: String -> String -> Html
        codeblock' []   = pre . code . toHtml
        codeblock' info = pre . flip (!) (class_ $ toValue $ "language-" ++ info) . code . toHtml

headings :: [(Int, Html -> Html)]
headings = [(1, h1), (2, h2), (3, h3), (4, h4), (5, h5), (6, h6)]

toHtml' :: B.ByteString -> Html
toHtml' = toHtml . B.unpack

toValue' :: B.ByteString -> AttributeValue
toValue' = toValue . B.unpack

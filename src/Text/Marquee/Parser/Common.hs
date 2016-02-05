module Text.Marquee.Parser.Common where

type BlockParser = StateT [Int] Parser
type InlineParser = StateT A.LinkMap Parser

indent :: Int -> BlockParser ()
indent n = modify ((:) n)

unindent :: BlockParser ()
unindent = modify (drop 1)

indentLevel :: BlockParser Int
indentLevel = liftM sum get


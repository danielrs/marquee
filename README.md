# Marquee

Markdown parser transpiler (source-to-source compiler) written in Haskell and parsec. It follows (or tries to follow) most of the [CommonMark][commonmark] especification.

### Purpose

I wrote this library for personal usage along with [Yesod][yesod]; however, it can be used by anyone.

### What needs to be done?

The lexer and parser follow most of the rules outlined at *CommonMark*, however, some parsing stills needs polishing:

1. Raw HTML parsing; nothing implemented until now.

2. Optimize the parser

### Structure of the library

All of the important code lies inside the `Text.Marquee` namespace.

`Text.Marquee.SyntaxTrees` includes all the intermediate representations of Markdown used by this library; more specifically the *Concrete Syntax Tree* (CST) and the *Abstract Syntax Tree* (AST).

`Text.Marquee.Writers` includes all the writers that take an AST and return something else, like a `String` or another intermediate representation.

`Text.Marquee.Parser` module is the lexer and parser of Markdown; it returns a CST or AST (one usually wants the AST)

[commonmark]: http://commonmark.org/
[yesod]: http://www.yesodweb.com/

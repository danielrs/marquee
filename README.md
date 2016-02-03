# Marquee

Markdown transpiler (source-to-source compiler) written in Haskell and attoparsec. It follows (or tries to follow) most of the [CommonMark][commonmark] especification.

### Purpose

I wrote this library for personal usage along with [Yesod][yesod]; however, it can be used by anyone.

### What needs to be done?

The parser follows most of the rules outlined at *CommonMark*, however, some parsing stills needs polishing:

1. Raw HTML parsing; nothing implemented until now.

### Structure of the library

All of the important code lies inside the `Text.Marquee` namespace.

`Text.Marquee.SyntaxTrees` includes all the intermediate representations of Markdown used by this library; more specifically the *Concrete Syntax Tree* (CST) and the *Abstract Syntax Tree* (AST).

`Text.Marquee.Writers` includes all the writers that take an AST and return something else, like a `String` or another intermediate representation.

`Text.Marquee.Parser` module is the lexer and parser of Markdown; it returns a CST or AST (one usually wants the AST)

### How to use it?

The library ships with *two* executables:

#### Minimal

[`app/Minimal.hs`][minimal]

A minimal working example of the library. It reads a file, creates the AST, and passes it to the HTML writer.

#### CLI Program

[`app/Main.hs`][cli]

A command line program that reads from any file (including standard input) and writes to any file (including standard output). Here's the help text for the program:

```
Marquee: Markdown transpiler

Usage: marquee [-i|--input INPUT_FILE] [-o|--output OUTPUT_FILE]
               [-f|--format FORMAT] [--html-page] [--html-title TITLE]
               [--html-css CSS_FILE]
  Reads Markdown and output something else

Available options:
  -h,--help                Show this help text
  -i,--input INPUT_FILE    Markdown file to read
  -o,--output OUTPUT_FILE  File to write the output to
  -f,--format FORMAT       The format of the output, supported formats are: html
  --html-page              Produce HTML as a full page (doctype, head, and body)
  --html-title TITLE       The title of the generated HTML page
  --html-css CSS_FILE      The CSS file that will be linked to the generated
                           HTML page
```

[commonmark]: http://commonmark.org/
[yesod]: http://www.yesodweb.com/
[minimal]: app/Minimal.hs
[cli]: app/Main.hs


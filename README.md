# Term Project

This project implements the term project of NSWI164 at MFF-UK.

Please do not copy as a solution to a similar task.

## Implementation choices

I chose haskell's algebraic ADTs for representing the language models as ADTs allow for very succinct specifications of context-free-grammar-like structures. Check [src/DSL/AST.hs](src/DSL/AST.hs) and [src/LilyPond/AST.hs](src/LilyPond/AST.hs)

> Few examples supporting the claim: `Maybe` can efficiently represent optional fields, `[]` can efficiently represent repeatable fields, and, on top of that, haskell greatly supports generic programming.

For parsing and lexing, I use haskell's *megaparsec* library, which allows for almost 1:1 (considering the length) transcription of grammar rules into an implementation of a parser or lexer. Check [src/DSL/Lexer.hs](src/DSL/Lexer.hs) and [src/DSL/Parser.hs](src/DSL/Parser.hs).

For prettyprinting, I use haskell's *prettyprinter* library, which, again, allows for almost 1:1 transcription of prettyprinting rules into an implementation of a prettyprinter. Check [src/LilyPond/Pretty.hs](src/LilyPond/Pretty.hs) and [src/NoteName/Pretty.hs](src/NoteName/Pretty.hs).

Translation from the DSL language to the LilyPond language is modelled using haskell's strict type system, which guides the implementation. The type system is involved in other parts of the implementation as well, but here is much more relevant to the development process (and for prettyprinting, the type-checks ensure completeness).

## How to build

```sh
make
```

or

```sh
cabal build
```

## How to run

```sh
make run
```

or

```sh
cabal run -v0
```

The program then reads from the stdin and outputs to stdout.

You can redirect the input/output to a pipe or set it to a desired file like so:

```sh
make run < in.dsl > out.lilypond
```

or

```sh
cat in.dsl | make run > out.lilypond
```

## How to test on the example from the assignment

```sh
make test
```

expected output: `SUCCESS`

## How to generate graphic pseudoUML representation of the language models

```sh
make generateGraphs
```

This will generate 3 graphviz file: [Combined.dot](Combined.dot), [DSL.dot](DSL.dot), [LilyPond.dot](LilyPond.dot)

- *(sums)* each custom ADT has a single output relation pointing to the actual instance, modelling the fact that there could be multiple possibilities and also making the memory semantics more explicit
- *(products)* all ADT fields are numbered starting from zero

Use `dot` program to transform the graphviz files into images/documents; or `xdot` (or similar) to view them interactively

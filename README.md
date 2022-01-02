# Term Project

This project implements the term project of NSWI164 at MFF-UK.

Please do not copy as a solution to a similar task.

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

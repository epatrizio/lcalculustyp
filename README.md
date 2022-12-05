# lcalculustyp - Lambda calculus typer and evaluator in OCaml

Here is a typer (and a evaluator) of lambda calculus.\
[Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) is at the base of
the very concept of any programming language
(the lambda calculus is [Turing complete](https://en.wikipedia.org/wiki/Turing_completeness)).
So, it's therefore relevant to do once in a lifetime a typer and an evaluator of lambda calculus ;)

*This project is part of type systems course of my professional training at Sorbonne University (2022).*

## Technical stack / choices

I chose an [OCaml language](https://ocaml.org) implementation,
with the standard [build system Dune](https://dune.build).

The Makefile contains all the base commands to compile and run (clean, build, exec).

## General informations

Running the program sequentially launches all lambda-terms of the [bin/lterms.ml](bin/lterms.ml) file.
Each lambda-term is :

1. Typed
2. Evaluated if typing is successful

All lambda-terms are constructed "by hand", there is no lexical and syntactic analyzer.\
To see a full example, this [compiler](https://github.com/epatrizio/comp2mzam) project
contains a lexical and syntactic analyser implemented in OCaml.

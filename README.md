# Arrow programming language

The Pirat language is based on the concept of arrows, as proposed by John Hughes. It focuses on how data flows through the program. This repository contains a compiler for the language. 

The resulting binaries run on Linux. The compilation depends on __nasm__ and on __ld__.

This project is part of my Matura thesis.

## Installation

There is a binary a /build/pirat directory.

If you want to compile the project yourself, you need to install __ghc__ and __cabal__. The recommended way to do so, is by using [__ghcup__](https://www.haskell.org/ghcup/). Then just go to the directory with the cloned git repository and run:
> cabal build

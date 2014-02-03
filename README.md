simplelang
==========

A simple lazy language compiled to C via STG [Peyton Jones & Salkild 1989]; implemented in Haskell and C. The goal is to build a simple, easy-to-understand compiler for a small, lazy core language (similar to GHC core) to use as a platform for experimentation with larger source languages.

Todo
----

* ~~Core => GCode compiler~~
* ~~GCode interpreter~~
* ~~GCode compiler (to C)~~
* ~~Replace G-Machine with STG [Peyton Jones & Salkild 1989]~~
* LLVM backend
* Extend core language to System-F [Reynolds 1974]

Usage
-----

    simplelang BACKEND SOURCEFILE

Backends:

* *-bviac* - Generate C code (from STG). Requires `RTS.h` and must be compiled with `RTS.c`.
* *-bpp* - Pretty print core code.
* *-bppstg* - Generate and pretty print STG code.

Directories
-----------

* examples
    * Example programs
* res
    * Compiler run-time files
* rts
    * Runtime system code (C)
* src
    * Compiler and interpreter source code (Haskell)

Modules
-------

* *AST* - Core language syntax tree and helpter functions.
* *Parsec* - Parsec parser for core.
* *PrettyPrinter* - Functions for pretty printing of core AST.
* *STG* - STG generation, pretty printing and compilation.
* *TypeCheck* - Type-checks core AST.

simplelang
==========

A simple lazy language running on the "G-Machine" abstract machine [Augustsson 1987, Johnsson 1987] implemented in Haskell and C. The goal is to build a simple, easy-to-understand compiler for a small, lazy core language (similar to GHC core) to use as a platform for experimentation with larger source languages.

Todo
----

* ~~Core => GCode compiler~~
* ~~GCode interpreter~~
* ~~GCode compiler (to C)~~
* Replace G-Machine with STG [Peyton Jones & Salkild 1989]
* LLVM backend
* Extend core language to System-F [Reynolds 1974]

Usage
-----

    simplelang BACKEND SOURCEFILE

Backends:

* *-bi* - Interpreter. Generates and interprets GCode, printing the final value only.
* *-biv* - Interpreter (verbose). Generates and interprets GCode, printing each state as it goes.
* *-bviac* - Generate C code (from GCode). Requires `RTS.h` and must be compiled with `RTS.c`.
* *-bpp* - Pretty print core code.
* *-bppgc* - Generate and pretty print GCode.

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
* *Compiler* - Generates GCode from core.
* *GMachine* - Defines GCode instructions.
* *GMachine.Interpreter* - Interpreter for GCode.
* *GMachine.ViaC* - Generates C code from GCode.
* *Parsec* - Parsec parser for core.
* *PrettyPrinter* - Functions for pretty printing of core AST.
* *TypeCheck* - Type-checks core AST.

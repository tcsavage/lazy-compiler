simplelang
==========

Usage:

    simplelang BACKEND SOURCEFILE

Backends:

* *-bi* - Interpreter. Generates and interprets GCode, printing the final value only.
* *-biv* - Interpreter (verbose). Generates and interprets GCode, printing each state as it goes.
* *-bviac* - Generate C code (from GCode). Requires `RTS.h` and must be compiled with `RTS.c`.
* *-bpp* - Pretty print core code.
* *-bppgc* - Generate and pretty print CGode.

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

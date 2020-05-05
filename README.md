## Compiler Utilities

This project collects together a bunch of utility modules and code-generation
tools that I've used over the years in various compiler projects.  The code is
written in Standard ML (as are the tools) and assumes the presence of the
SML/NJ library.

### Roadmap

The code is organized into two subdirectories: `libsrc`, which contains the various SML
utility modules, and `tools`, which contains some code generation tools.

#### Libraries

* [CodeTemplates](libsrc/CodeTemplates/README.md) <a name="make-fragments"></a> --
    Infrastructure for supporting textual code snippits with substitutions.  This
    code supports the [Make Fragments](#make-fragments) tool descrbied below.

* [Constant arithemetic](libsrc/ConstArith/README.md) -- Infrastructure for
    constant-folding integer arithmetic at different precisions.

* [Errors](libsrc/Errors/README.md) -- error tracking for
    [ML-LPT](http://smlnj.org/doc/ml-lpt/manual.pdf) based parsers

* [Floats](libsrc/Floats/README.md) -- A representation of floating-point
    literals plus support for converting to IEEE binary representations.

* [Logging](libsrc/Logging/README.md) -- Utility code for log messages and
    for timing compiler phases.

* Stamps -- unique stamps for tagging identifiers and other semantic objects

#### Tools

* [MakeFragments](tools/MakeFragments/README.md) <a name="make-fragments"></a> --
    This directory contains tools for converting source-code fragments into
    SML string constants in an SML module.

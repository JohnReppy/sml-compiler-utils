## Compiler Utilities

This project collects together a bunch of utiloity modules and code-generation
tools that I've used over the years in various compiler projects.  The code is
written in Standard ML (as are the tools) and assumes the presence of the
SML/NJ library.

### Roadmap

Thecode is organized into two subdirectories: `libsrc`, which contains the various SML
utility modules, and `tools`, which contains some code generation tools.

#### Libraries

* Errors -- error tracking for [ML-LPT](http://smlnj.org/doc/ml-lpt/manual.pdf) based parsers

* Stamps -- unique stamps for tagging identifiers

* Floats -- A representation of floating-point literals plus support for converting
  to IEEE binary representations.

* Logging -- Utility code for log messages and for timing compiler phases

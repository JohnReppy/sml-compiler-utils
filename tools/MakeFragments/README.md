## Compiler Utilities: Make Fragments

This directory contains the implementation of a tool for converting
source-code fragments contained in source files to string literals
in an SML structure that are suitable for use by the
[code templates](../../libsrc/CodeTemplates/README.md) library.

### Usage

Program to generate a file `"fragments.sml"` containing a fragments structure
from a `CATALOG` file.  A `CATALOG` file has the following layout

     <structure name>
     <input file>    <fragment name>
     <input file>    <fragment name>
     ...
     <input file>    <fragment name>

The resulting file (named `fragments.sml`) will contain a structure with the given
name; the structure consists of named fragments.  Two kinds of input files are
supported.  If the input file has a `".in"` suffix, then it is converted to an
SML literal string in the output file.  If it has a `".json"` suffix, then it
is parsed as a JSON value (see the SML/NJ JSON library) and the resulting
value in the output will be SML code that defines the corresponding `JSON.value`
value.

### Wrapper scripts

When in a compiler, I typically wrap the SML program with scripts that run
the `MkFrags.mkFragments` and `MkFrags.mkMakefile` functions with the appropriate
arguments.  When using SML/NJ, I just compile the program each time I run it
as follows:

````sh
#!/bin/sh
#
# wrapper script for MkFrags.mkFragments function
#

PROG=mkfrags

if [ $# != 1 ] ; then
  echo "usage: $PROG.sh <dir>"
  exit 1
fi

DIR=$1

SRC=/path/to/make/fragments/source/sources.cm

sml $SRC 2> /dev/null 1>/dev/null <<XXXX
MkFrags.mkFragments "$DIR";
XXXX
exit $?
````

For MLton, I add a `Makefile` to compile an executable (see `main.sml`), and
run make in the wrapper script to avoid the slow compile times of MLton.

## Compiler Utilities: Code Templates

This directory contains support for inserting textual code fragments
with substitutions into generated files.  The associated
[make-fragments](../../tools/MakeFragments/README.md) tool can be used
to generate the fragment strings from source files.

The basic operation is

````sml
CodeTemplate.expand substitutions text
````

which expands the string `text` by replacing *placeholders* with their expansion as
specified in the list of id-value pairs `substitutions`.  Placeholders in `text`
have the syntax `@`<id>`@` and are replaced with the string associated with <id>
in the list `substitutions`.  If <id> is empty, then no substitution
is applied, instead the `"@@"` is replaced by `"@"`.

I often use this mechanism to handle boilerplate code in compilers
that generate source code in a language like SML, C, or C++.

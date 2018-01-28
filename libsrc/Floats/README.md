## Compiler Utilities: Floats

This directory contains a representation of floating-point literals that
can support IEEE features, such as -0.0, +/- infinity, and NaNs.  There
is also code to support generating a binary representation of a literal
at various different precisions (16, 32, 64, 128, and 256 bits) according
to the IEEE encoding.  There is also code to support generating literals
that are compatible with the LLVM IR.

### Roadmap

* `README.md` -- this file
* `float-lit.sml` -- the `FloatLit` structure
* `float-to-bits-fn.sml` -- includes the `FloatToBitsFn` functor, as well
    as the `IEEE_FLOAT_PARAMS` signature its instances
* `float-to-bits-sig.sml` -- the `FLOAT_TO_BITS` signature
* `float-to-llvm-ir.sml` -- the `FloatToLLVMIR` structure
* `float16-to-llvm.sml` -- the `Float16ToLLVM` structure
* `float32-to-llvm.sml` -- the `Float32ToLLVM` structure
* `sources.cm` -- CM file for compiling the code
* `test32.sml` -- tests for the 32-bit encoding
* `test64.sml` -- tests for the 64-bit encoding

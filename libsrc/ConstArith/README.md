## Compiler Utilities: Constant Arithmetic

Many compilers perform compile-time arithmetic on constants as an optimization.
While such optimizations may seem trivial to implement, there are a number of
pesky details that must be addressed.  These include supporting varying precisions,
signed vs. unsigned operations, and wrapping vs. trapping on overflow.  This
library provides a collection of modules that support these optimizations and
which support a choice of semantics for arithmetic operations.

### The API

Our basic assumption is that compile-time integer constants are represented as
the **SML** type `IntInf.int` (*i.e.*, arbitrary-precision integers).

Operations are grouped into three signatures:

* `BITWISE_CONST_ARITH` -- these operations implement bitwise operations.  They
  support both signed and unsigned values and interpret negative arguments as
  having a 2's complement representation in bits.  For bitwidth of WID, values
  should be in the range -2^(WID-1)^ to 2^WID^-1 (note the asymmetry).

* `SIGNED_CONST_ARITH` -- these operations implement signed arithmetic.  For
  a bitwidth of WID, values should be in the range -2^(WID-1)^ to 2^(WID-1)^-1.

* `UNSIGNED_CONST_ARITH` -- these operations implement unsigned arithmetic.  For
  a bitwidth of WID, values should be in the range 0 to 2^WID^-1.

The `CONST_ARITH` signature is a union of these three signatures.

### Implementations

Each of the three signatures has two implementations: a *trapping* implementation
where the `Overflow` exception is raised when values are two large to be represented
in the specified number of bits, and a *wrapping* implementation, where results are
narrowed to the specified precision.

````sml
structure BitwiseTrappingArith : BITWISE_CONST_ARITH
structure BitwiseWrappingArith : BITWISE_CONST_ARITH

structure SignedTrappingArith : SIGNED_CONST_ARITH
structure SignedWrappingArith : SIGNED_CONST_ARITH

structure UnsignedTrappingArith : UNSIGNED_CONST_ARITH
structure UnsignedWrappingArith : UNSIGNED_CONST_ARITH
````

In addition, the `ConstArithGlueFn` functor can be used to glue three modules into an
implementation of the `CONST_ARITH` signature.

````sml
functor ConstArithGlueFn (
    structure B : BITWISE_CONST_ARITH
    structure S : SIGNED_CONST_ARITH
    structure U : UNSIGNED_CONST_ARITH
  ) : CONST_ARITH
````

Lastly, there are functors that wrap the three types of arithmetic modules with
error checking of the arguments.  These are meant to be used when one wants to
include internal consistency checking in one's compiler.

````sml
functor CheckBitwiseArithFn (A : BITWISE_CONST_ARITH) : BITWISE_CONST_ARITH

functor CheckSignedArithFn (A : SIGNED_CONST_ARITH) : SIGNED_CONST_ARITH

functor CheckUnsignedArithFn (A : UNSIGNED_CONST_ARITH) : UNSIGNED_CONST_ARITH
````

### Examples

In languages like **C** and **C++**, integer arithmetic is non-trapping.  We can define
an instatiation of the `CONST_ARITH` interface for these languages by using the wrapping
implementations:

````sml
structure CArith = ConstArithGlueFn (
    structure B = BitwiseWrappingArith
    structure S = SignedWrappingArith
    structure U = UnsignedWrappingArith)
````

In Standard ML, however, the semantics of arithmetic is more complicated, since signed operations
are trapping, while unsigned operations (*i.e.*, `word` operations) wrap.

````sml
structure SMLArith = ConstArithGlueFn (
    structure B = BitwiseWrappingArith
    structure S = SignedTrappingArith
    structure U = UnsignedWrappingArith)
````


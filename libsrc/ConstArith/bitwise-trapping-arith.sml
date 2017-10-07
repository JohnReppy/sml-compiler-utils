(* bitwise-trapping-arith.sml
 *
 * Operations for constant-folding bitwise operations on constant integers,
 * where the shift operations trap on overflow.
 *
 * COPYRIGHT (c) 2017 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * This file is part of the SML Compiler Utilities, which can be found at
 *
 *      https://github.com/JohnReppy/sml-compiler-utils
 *)

structure BitwiseTrappingArith : BITWISE_CONST_ARITH =
  struct

  (* we use arbitrary-precision integers to represent constant values *)
    type t = IntInf.int

  (* bit-widths are represented as integers *)
    type width = int

  (* we mostly use the Wrapping implementations, since most bitwise operations
   * cannot overflow.
   *)
    val bAnd = BitwiseWrappingArith.bAnd
    val bOr  = BitwiseWrappingArith.bOr
    val bXor = BitwiseWrappingArith.bXor
    val bNot = BitwiseWrappingArith.nNot

    val bLShiftRight = BitwiseWrappingArith.bLShiftRight
    val bAShiftRight = BitwiseWrappingArith.bAShiftRight

  (* logical left-shift operation.  If the value being shifted is negative,
   * it will first be converted to the positive value with the same bit
   * representation before being shifted.  We raise Overflow if the result
   * is not representable.
   *)
    fun bShiftLeft (wid, a, b) = ??

  end

(* signed-trapping-arith.sml
 *
 * Implements signed, trapping arithmetic.
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

structure SignedTrappingArith : SIGNED_CONST_ARITH =
  struct

    type t = IntInf.int
    type width = int

    fun pow2 w = IntInf.<<(1, Word.fromInt w)

  (* narrow the representation of n to `wid` bits, which just means checking if it is
   * representable and raising Overflow if not.
   *)
    fun sNarrow (wid, n) = let
          val limit = pow2(wid - 1)
          in
	    if (n < ~limit) orelse (limit <= n)
	      then raise Overflow
              else n
	  end

    fun toSigned (wid, a) = if a < pow2(wid - 1)
	  then a
	  else IntInf.notb a + 1

    fun sAdd (wid, a, b) = sNarrow (wid, a + b)
    fun sSub (wid, a, b) = sNarrow (wid, a - b)
    fun sMul (wid, a, b) = sNarrow (wid, a * b)
    fun sDiv (wid, a, b) = sNarrow (wid, a div b)
    fun sMod (wid, a, b) = sNarrow (wid, a mod b)
    fun sQuot (wid, a, b) = sNarrow (wid, IntInf.quot(a, b))
    fun sRem (wid, a, b) = sNarrow (wid, IntInf.mod(a, b))
    fun sNeg (wid, a) = sNarrow (wid, ~a)
    fun sAbs (wid, a) = if (a < 0) then sNarrow (wid, ~a) else a

  end

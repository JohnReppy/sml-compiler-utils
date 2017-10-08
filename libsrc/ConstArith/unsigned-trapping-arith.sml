(* unsigned-trapping-arith.sml
 *
 * Implements unsigned arithmetic.  Results that are out of range wrap (e.g.,
 * max-int + 1 = 0) result in the Overflow exception being raised.
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
 * This code is part of the SML Compiler Utilities, which can be found at
 *
 *      https://github.com/JohnReppy/sml-compiler-utils
 *
 * COPYRIGHT (c) 2017 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure UnsignedTrappingArith : UNSIGNED_CONST_ARITH =
  struct

    type t = IntInf.int
    type width = int

    fun pow2 w = IntInf.<<(1, Word.fromInt w)

    fun uNarrow (wid, n) = if (n < 0) orelse (pow2 wid <= n)
	  then raise Overflow
	  else n

    fun toUnsigned (wid, n) = IntInf.andb(n, pow2 wid - 1)

    fun uAdd (wid, a, b) = uNarrow (wid, a + b)
    fun uSub (wid, a, b) = uNarrow (wid, a - b)
    fun uMul (wid, a, b) = uNarrow (wid, a * b)
    fun uDiv (wid, a, b) = uNarrow (wid, IntInf.quot(a, b))
    fun uMod (_, 0, 0) = raise Div (* workaround for bug in SML/NJ pre 110.82 *)
      | uMod (wid, a, b) = uNarrow (wid, IntInf.rem(a, b))

  (* 2's complement of unsigned argument as unsigned value *)
    fun uNeg (wid, a) = let
	  val mask = pow2 wid - 1
	  in
	    IntInf.andb(mask, IntInf.xorb(mask, a) + 1)
	  end

  (* unsigned left-shift operation. *)
    fun uShL (wid, a, b) =
	  uNarrow (wid, IntInf.<<(a, Word.fromLargeInt(IntInf.min(b, IntInf.fromInt wid))))

  (* unsigned right-shift operation. Shift amounts that are >= wid result in zero. *)
    fun uShR (wid, a, b) =
          if (b >= IntInf.fromInt wid)
            then 0
            else IntInf.~>>(a, Word.fromLargeInt b)

  end

(* bitwise-const-arith.sml
 *
 * Operations for constant-folding bitwise operations on constant integers.
 *
 * COPYRIGHT (c) 2025 John Reppy (https://cs.uchicago.edu/~jhr)
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
 *)

structure BitwiseConstArith : BITWISE_CONST_ARITH =
  struct

  (* we use arbitrary-precision integers to represent constant values *)
    type t = IntInf.int

  (* bit-widths are represented as integers *)
    type width = int

    fun pow2 w = IntInf.<<(1, Word.fromInt w)

    fun narrow (wid, n) = IntInf.andb(n, pow2 wid - 1)

    val toUnsigned = narrow
    fun toSigned (wid, a) = if a < pow2(wid - 1)
	  then a
	  else a - pow2 wid

    fun bAnd (wid, a, b) = narrow (wid, IntInf.andb(a, b))
    fun bOr (wid, a, b) = narrow (wid, IntInf.orb(a, b))
    fun bXor (wid, a, b) = narrow (wid, IntInf.xorb(a, b))
    fun bNot (wid, a) = narrow (wid, IntInf.xorb(a, pow2 wid - 1))

    fun bRotateL (wid, a, b) = let
          val w = Word.fromInt wid
          val a = if (a < 0) then toUnsigned(wid, a) else a
          in
            case Word.fromLargeInt(b mod IntInf.fromInt wid)
             of 0w0 => a
              | n => narrow (wid, IntInf.orb(IntInf.<<(a, n), IntInf.~>>(a, w-n)))
            (* end case *)
          end

    fun bRotateR (wid, a, b) = let
          val w = Word.fromInt wid
          val a = if (a < 0) then toUnsigned(wid, a) else a
          in
            case Word.fromLargeInt(b mod IntInf.fromInt wid)
             of 0w0 => a
              | n => narrow (wid, IntInf.orb(IntInf.<<(a, w-n), IntInf.~>>(a, n)))
            (* end case *)
          end

    (* NOTE: we use the O(wid) algorithms for the counting operators because they
     * are simple and we do not expect that these operations are frequently used
     * in constant folding.
     *)

    fun bCountOnes (wid, 0) = 0
      | bCountOnes (wid, a) = let
          val a = if (a < 0) then toUnsigned(wid, a) else a
          fun lp (0, n) = IntInf.fromInt n
            | lp (a, n) = if (IntInf.andb(a, 1) = 0)
                then lp (IntInf.~>>(a, 0w1), n)
                else lp (IntInf.~>>(a, 0w1), n+1)
          in
            lp (a, 0)
          end

    fun bCountLeadingZeros (wid, 0) = IntInf.fromInt wid
      | bCountLeadingZeros (wid, a) = let
          val a = if (a < 0) then toUnsigned(wid, a) else a
          fun lp (0, n) = IntInf.fromInt n
            | lp (bit, n) = if (IntInf.andb(bit, a) = 0)
                then lp (IntInf.~>>(bit, 0w1), n+1)
                else IntInf.fromInt n
          in
            lp (pow2 (wid - 1), 0)
          end

    fun bCountTrailingZeros (wid, 0) = IntInf.fromInt wid
      | bCountTrailingZeros (wid, a) = let
          val a = if (a < 0) then toUnsigned(wid, a) else a
          fun lp (bit, n) = if (n = wid)
                  then IntInf.fromInt n
                else if (IntInf.andb(bit, a) = 0)
                  then lp (IntInf.<<(bit, 0w1), n+1)
                  else IntInf.fromInt n
          in
            lp (1, 0)
          end

  end

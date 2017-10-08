(* check-bitwise-arith-fn.sml
 *
 * A wrapper functor for implementations of the BITWISE_CONST_ARITH signature,
 * which adds validity checking of the arguments.
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
 *)

functor CheckBitwiseArithFn (

  (* implementation to be checked *)
    structure A : BITWISE_CONST_ARITH

  (* should be the name of the structure that A is bound to with trailing ".", but ""
   * is also okay.
   *)
    val qual : string

  (* function for reporting the error; this function should raise the appropriate exception *)
    val error : string -> 'a

  ) : BITWISE_CONST_ARITH =
  struct

    type t = A.t
    type width = A.width

    fun pow2 w = IntInf.<<(1, Word.fromInt w)

    fun chkWid err w = if (w < 1) then err() else w
    fun chkArg err w = let val limit = pow2 w
	  in
	    fn n => if (n < 0) orelse (limit <= n) then err() else n
	  end

    fun chk1 name f (w, arg) = let
	  fun err () = error(concat[
		  "'", qual, name, "(", Int.toString w, ", ", IntInf.toString arg, ")'"
		])
	  in
	    f (chkWid err w, chkArg err w arg)
	  end

    fun chk2 name f (w, arg1, arg2) = let
	  fun err () = error(concat[
		  "'", qual, name, "(", Int.toString w, ", ", IntInf.toString arg1, ", ",
		  IntInf.toString arg2, ")'"
		])
	  val chkArg = chkArg err w
	  in
	    f (chkWid err w, chkArg arg1, chkArg arg2)
	  end

    val bAnd = chk2 "bAnd" A.bAnd
    val bOr  = chk2 "bOr" A.bOr
    val bXor = chk2 "bXor" A.bXor
    val bNot = chk1 "nNot" A.bNot

  end

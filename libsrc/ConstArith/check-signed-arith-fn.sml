(* check-signed-arith-fn.sml
 *
 * A wrapper functor for implementations of the SIGNED_CONST_ARITH signature,
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
 * This file is part of the SML Compiler Utilities, which can be found at
 *
 *      https://github.com/JohnReppy/sml-compiler-utils
 *)

functor CheckSignedArithFn (

  (* implementation to be checked *)
    structure A : SIGNED_CONST_ARITH

  (* should be the name of the structure that A is bound to with trailing ".", but ""
   * is also okay.
   *)
    val qual : string

  (* function for reporting the error; this function should raise the appropriate exception *)
    val error : string -> 'a

  ) : SIGNED_CONST_ARITH =
  struct

    type t = A.t
    type width = A.width

    fun pow2 w = IntInf.<<(1, Word.fromInt w)

    fun chkWid err w = if (w < 1) then err() else w
    fun chkArg err (w, n) = if (n < 0) orelse (pow2 w <= n) then err() else n

    fun chk1 name f (w, arg) = let
	  fun err () = error(concat[
		  "'", qual, name, "(", Int.toString w, ", ", IntInf.toString arg, ")'"
		])
	  in
	    fn (w, arg) => f (chkWid err w, chkArg err arg)
	  end

    fun chk2 name f (w, arg1, arg2) = let
	  fun err () = error(concat[
		  "'", qual, name, "(", Int.toString w, ", ", IntInf.toString arg1, ", "
		  IntInf.toString arg2, ")'"
		])
	  in
	    fn (w, arg) => f (chkWid err w, chkArg err arg1, chkArg err arg2)
	  end

  (* narrow a signed-constant to fit within the range -2^(WID-1)^..2^(WID-1)^-1.
   * Depending on the semantics structure implementing this signature, this operation
   * may raise Overflow on values that are outside the range -2^(WID-1)^..pow2^(WID-1)^.
   *)
    val sNarrow : width * t -> t

  (* converts values in range 0..pow2(width)-1 to -pow2(width-1)..pow2(width-1)-1 *)
    val toSigned : width * t -> t

    val sAdd  = chk2 "sAdd" A.sAdd
    val sSub  = chk2 "sSub" A.sSub
    val sMul  = chk2 "sMul" A.sMul
    val sDiv  = chk2 "sDiv" A.sDiv
    val sMod  = chk2 "sMod" A.sMod
    val sQuot = chk2 "sQuot" A.sQuot
    val sRem  = chk2 "sRem" A.sRem
    val sNeg  = chk1 "sNeg" A.sNeg
    val sAbs  = chk1 "sAbs" A.sAbs

  end

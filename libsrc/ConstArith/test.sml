(* test.sml
 *
 * Tests for the Constant Arithmetic library.
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

structure TestUtil =
  struct

    datatype value = INT of IntInf.int | BOOL of bool | OVFL_EXN | DIV_EXN

    fun valueToString (INT n) = IntInf.toString n
      | valueToString (BOOL b) = Bool.toString b
      | valueToString OVFL_EXN = "[Overflow]"
      | valueToString DIV_EXN = "[Div]"

    datatype result
      = OK
      | BAD of {expected : value, actual : value}
      | EXN of exn

    fun resultToString OK = "okay"
      | resultToString (BAD{expected, actual}) = String.concat [
	    "failed: expected ", valueToString expected, ", but got ",
	    valueToString actual
	  ]
      | resultToString (EXN ex) = "failed: unexpected exception " ^ exnName ex

    fun eval f arg = INT(f arg)
	  handle Overflow => OVFL_EXN
	       | Div => DIV_EXN

    fun checkApp f arg expected = let
	  val res = eval f arg
	  in
	    if (expected = res)
	      then OK
	      else BAD{expected = expected, actual = res}
	  end
	    handle ex => EXN ex

    fun pr msg = (
	  TextIO.output(TextIO.stdOut, concat msg);
	  TextIO.flushOut TextIO.stdOut)

    fun check1 strId funId (f : int * IntInf.int -> IntInf.int) (w, arg, res) =
	  pr [
	      StringCvt.padRight #" " 31 (strId ^ ":"), " ",
	      StringCvt.padLeft #" " 10 (concat[funId, " ", IntInf.toString arg]),
	      " == ", valueToString res, " ", resultToString(checkApp f (w, arg) res), "\n"
	    ]

    fun check2 strId funId (f : int * IntInf.int * IntInf.int -> IntInf.int) (w, arg1, arg2, res) =
	  pr [
	      StringCvt.padRight #" " 31 (strId ^ ":"), " ",
	      StringCvt.padLeft #" " 10 (concat[
		  IntInf.toString arg1, " ", funId, " ", IntInf.toString arg2
		]),
	      " == ", valueToString res, " ", resultToString(checkApp f (w, arg1, arg2) res), "\n"
	    ]

  end; (* TestUtil *)

(* tests that are common across both Trapping and Wrapping implementations
 * of the Bitwise operations.
 *)
structure BitwiseTests =
  struct
    local
      datatype z = datatype TestUtil.value
      type test_case = int * IntInf.int * IntInf.int * TestUtil.value
    in
    val bAndTests : test_case list = [
	    (3, 0, 0, INT 0),   (3, 0, 1, INT 0),   (3, 0, 2, INT 0),   (3, 0, 3, INT 0),
	    (3, 0, 4, INT 0),   (3, 0, 5, INT 0),   (3, 0, 6, INT 0),   (3, 0, 7, INT 0),
	    (3, 1, 0, INT 0),   (3, 1, 1, INT 1),   (3, 1, 2, INT 0),   (3, 1, 3, INT 1),
	    (3, 1, 4, INT 0),   (3, 1, 5, INT 1),   (3, 1, 6, INT 0),   (3, 1, 7, INT 1),
	    (3, 2, 0, INT 0),   (3, 2, 1, INT 0),   (3, 2, 2, INT 2),   (3, 2, 3, INT 2),
	    (3, 2, 4, INT 0),   (3, 2, 5, INT 0),   (3, 2, 6, INT 2),   (3, 2, 7, INT 2),
	    (3, 3, 0, INT 0),   (3, 3, 1, INT 1),   (3, 3, 2, INT 2),   (3, 3, 3, INT 3),
	    (3, 3, 4, INT 0),   (3, 3, 5, INT 1),   (3, 3, 6, INT 2),   (3, 3, 7, INT 3),
	    (3, 4, 0, INT 0),   (3, 4, 1, INT 0),   (3, 4, 2, INT 0),   (3, 4, 3, INT 0),
	    (3, 4, 4, INT 4),   (3, 4, 5, INT 4),   (3, 4, 6, INT 4),   (3, 4, 7, INT 4),
	    (3, 5, 0, INT 0),   (3, 5, 1, INT 1),   (3, 5, 2, INT 0),   (3, 5, 3, INT 1),
	    (3, 5, 4, INT 4),   (3, 5, 5, INT 5),   (3, 5, 6, INT 4),   (3, 5, 7, INT 5),
	    (3, 6, 0, INT 0),   (3, 6, 1, INT 0),   (3, 6, 2, INT 2),   (3, 6, 3, INT 2),
	    (3, 6, 4, INT 4),   (3, 6, 5, INT 4),   (3, 6, 6, INT 6),   (3, 6, 7, INT 6),
	    (3, 7, 0, INT 0),   (3, 7, 1, INT 1),   (3, 7, 2, INT 2),   (3, 7, 3, INT 3),
	    (3, 7, 4, INT 4),   (3, 7, 5, INT 5),   (3, 7, 6, INT 6),   (3, 7, 7, INT 7)
	  ]
    val bOrTests : test_case list = [
	    (3, 0, 0, INT 0),   (3, 0, 1, INT 1),   (3, 0, 2, INT 2),   (3, 0, 3, INT 3),
	    (3, 0, 4, INT 4),   (3, 0, 5, INT 5),   (3, 0, 6, INT 6),   (3, 0, 7, INT 7),
	    (3, 1, 0, INT 1),   (3, 1, 1, INT 1),   (3, 1, 2, INT 3),   (3, 1, 3, INT 3),
	    (3, 1, 4, INT 5),   (3, 1, 5, INT 5),   (3, 1, 6, INT 7),   (3, 1, 7, INT 7),
	    (3, 2, 0, INT 2),   (3, 2, 1, INT 3),   (3, 2, 2, INT 2),   (3, 2, 3, INT 3),
	    (3, 2, 4, INT 6),   (3, 2, 5, INT 7),   (3, 2, 6, INT 6),   (3, 2, 7, INT 7),
	    (3, 3, 0, INT 3),   (3, 3, 1, INT 3),   (3, 3, 2, INT 3),   (3, 3, 3, INT 3),
	    (3, 3, 4, INT 7),   (3, 3, 5, INT 7),   (3, 3, 6, INT 7),   (3, 3, 7, INT 7),
	    (3, 4, 0, INT 4),   (3, 4, 1, INT 5),   (3, 4, 2, INT 6),   (3, 4, 3, INT 7),
	    (3, 4, 4, INT 4),   (3, 4, 5, INT 5),   (3, 4, 6, INT 6),   (3, 4, 7, INT 7),
	    (3, 5, 0, INT 5),   (3, 5, 1, INT 5),   (3, 5, 2, INT 7),   (3, 5, 3, INT 7),
	    (3, 5, 4, INT 5),   (3, 5, 5, INT 5),   (3, 5, 6, INT 7),   (3, 5, 7, INT 7),
	    (3, 6, 0, INT 6),   (3, 6, 1, INT 7),   (3, 6, 2, INT 6),   (3, 6, 3, INT 7),
	    (3, 6, 4, INT 6),   (3, 6, 5, INT 7),   (3, 6, 6, INT 6),   (3, 6, 7, INT 7),
	    (3, 7, 0, INT 7),   (3, 7, 1, INT 7),   (3, 7, 2, INT 7),   (3, 7, 3, INT 7),
	    (3, 7, 4, INT 7),   (3, 7, 5, INT 7),   (3, 7, 6, INT 7),   (3, 7, 7, INT 7)
	  ]
    val bXorTests : test_case list = [
	    (3, 0, 0, INT 0),   (3, 0, 1, INT 1),   (3, 0, 2, INT 2),   (3, 0, 3, INT 3),
	    (3, 0, 4, INT 4),   (3, 0, 5, INT 5),   (3, 0, 6, INT 6),   (3, 0, 7, INT 7),
	    (3, 1, 0, INT 1),   (3, 1, 1, INT 0),   (3, 1, 2, INT 3),   (3, 1, 3, INT 2),
	    (3, 1, 4, INT 5),   (3, 1, 5, INT 4),   (3, 1, 6, INT 7),   (3, 1, 7, INT 6),
	    (3, 2, 0, INT 2),   (3, 2, 1, INT 3),   (3, 2, 2, INT 0),   (3, 2, 3, INT 1),
	    (3, 2, 4, INT 6),   (3, 2, 5, INT 7),   (3, 2, 6, INT 4),   (3, 2, 7, INT 5),
	    (3, 3, 0, INT 3),   (3, 3, 1, INT 2),   (3, 3, 2, INT 1),   (3, 3, 3, INT 0),
	    (3, 3, 4, INT 7),   (3, 3, 5, INT 6),   (3, 3, 6, INT 5),   (3, 3, 7, INT 4),
	    (3, 4, 0, INT 4),   (3, 4, 1, INT 5),   (3, 4, 2, INT 6),   (3, 4, 3, INT 7),
	    (3, 4, 4, INT 0),   (3, 4, 5, INT 1),   (3, 4, 6, INT 2),   (3, 4, 7, INT 3),
	    (3, 5, 0, INT 5),   (3, 5, 1, INT 4),   (3, 5, 2, INT 7),   (3, 5, 3, INT 6),
	    (3, 5, 4, INT 1),   (3, 5, 5, INT 0),   (3, 5, 6, INT 3),   (3, 5, 7, INT 2),
	    (3, 6, 0, INT 6),   (3, 6, 1, INT 7),   (3, 6, 2, INT 4),   (3, 6, 3, INT 5),
	    (3, 6, 4, INT 2),   (3, 6, 5, INT 3),   (3, 6, 6, INT 0),   (3, 6, 7, INT 1),
	    (3, 7, 0, INT 7),   (3, 7, 1, INT 6),   (3, 7, 2, INT 5),   (3, 7, 3, INT 4),
	    (3, 7, 4, INT 3),   (3, 7, 5, INT 2),   (3, 7, 6, INT 1),   (3, 7, 7, INT 0)
	  ]
    val bNotTests : (int * IntInf.int * TestUtil.value) list = [
	    (3, 0, INT 7),   (3, 1, INT 6),   (3, 2, INT 5),   (3, 3, INT 4),
	    (3, 4, INT 3),   (3, 5, INT 2),   (3, 6, INT 1),   (3, 7, INT 0)
	  ]
    end
  end

structure TestBitwiseTrapping =
  struct
    local
      datatype z = datatype TestUtil.value
      structure A = BitwiseTrappingArith
      val check1 = TestUtil.check1 "BitwiseTrappingArith"
      val check2 = TestUtil.check2 "BitwiseTrappingArith"
      val bAnd = check2 "AND" A.bAnd
      val bOr  = check2 "OR" A.bOr
      val bXor = check2 "XOR" A.bXor
      val bNot = check1 "NOT" A.bNot
(*
      val bLShiftRight : width * t * t -> t
      val bAShiftRight : width * t * t -> t
      val bShiftLeft   : width * t * t -> t
*)
    in
    val _ = List.app bAnd BitwiseTests.bAndTests
    val _ = List.app bOr BitwiseTests.bOrTests
    val _ = List.app bXor BitwiseTests.bXorTests
    val _ = List.app bNot BitwiseTests.bNotTests
    end (* local *)
  end (* TestBitwiseTrapping *)

structure TestBitwiseWrapping =
  struct
    local
      datatype z = datatype TestUtil.value
      structure A = BitwiseWrappingArith
      val check1 = TestUtil.check1 "BitwiseWrappingArith"
      val check2 = TestUtil.check2 "BitwiseWrappingArith"
      val bAnd = check2 "AND" A.bAnd
      val bOr  = check2 "OR" A.bOr
      val bXor = check2 "XOR" A.bXor
      val bNot = check1 "NOT" A.bNot
(*
      val bLShiftRight : width * t * t -> t
      val bAShiftRight : width * t * t -> t
      val bShiftLeft   : width * t * t -> t
*)
    in
    val _ = List.app bAnd BitwiseTests.bAndTests
    val _ = List.app bOr BitwiseTests.bOrTests
    val _ = List.app bXor BitwiseTests.bXorTests
    val _ = List.app bNot BitwiseTests.bNotTests
    end (* local *)
  end (* TestBitwiseWrapping *)

structure TestSignedTrapping =
  struct
    local
      datatype z = datatype TestUtil.value
      structure A = SignedTrappingArith
      val check1 = TestUtil.check1 "SignedWrappingArith"
      val check2 = TestUtil.check2 "SignedWrappingArith"
      val narrow = check1 "sNarrow" A.sNarrow
      val toSigned = check1 "toSigned" A.toSigned
      val add = check2 "+" A.sAdd
      val sub = check2 "-" A.sSub
      val mul = check2 "*" A.sMul
      val div' = check2 "div" A.sDiv
      val mod' = check2 "mod" A.sMod
      val quot = check2 "quot" A.sQuot
      val rem = check2 "rem" A.sRem
      val neg = check1 "~" A.sNeg
      val abs = check1 "abs" A.sAbs
    in
(* TODO *)
    end (* local *)
  end (* TestSignedTrapping *)

structure TestSignedWrapping =
  struct
    local
      datatype z = datatype TestUtil.value
      structure A = SignedWrappingArith
      val check1 = TestUtil.check1 "SignedWrappingArith"
      val check2 = TestUtil.check2 "SignedWrappingArith"
      val narrow = check1 "sNarrow" A.sNarrow
      val toSigned = check1 "toSigned" A.toSigned
      val add = check2 "+" A.sAdd
      val sub = check2 "-" A.sSub
      val mul = check2 "*" A.sMul
      val div' = check2 "div" A.sDiv
      val mod' = check2 "mod" A.sMod
      val quot = check2 "quot" A.sQuot
      val rem = check2 "rem" A.sRem
      val neg = check1 "~" A.sNeg
      val abs = check1 "abs" A.sAbs
    in
(* TODO *)
    end (* local *)
  end (* TestSignedWrapping *)

structure TestUnsignedTrapping =
  struct
    local
      datatype z = datatype TestUtil.value
      structure A = UnsignedTrappingArith
      val check1 = TestUtil.check1 "UnsignedTrappingArith"
      val check2 = TestUtil.check2 "UnsignedTrappingArith"
      val narrow = check1 "uNarrow" A.uNarrow
      val toUnsigned = check1 "toUnsigned" A.toUnsigned
      val add = check2 "+" A.uAdd
      val sub = check2 "-" A.uSub
      val mul = check2 "*" A.uMul
      val div' = check2 "div" A.uDiv
      val mod' = check2 "mod" A.uMod
      val neg = check1 "~" A.uNeg
    in
    val _ = List.app narrow [
	    (3, 0, INT 0), (3, 1, INT 1), (3, 2, INT 2), (3, 3, INT 3), (3, 4, INT 4),
	    (3, 5, INT 5), (3, 6, INT 6), (3, 7, INT 7), (3, 8, OVFL_EXN), (3, 9, OVFL_EXN)
	  ]
    val _ = List.app toUnsigned [
	    (3, ~5, INT 3), (3, ~4, INT 4), (3, ~3, INT 5), (3, ~2, INT 6), (3, ~1, INT 7),
	    (3,  0, INT 0), (3,  1, INT 1), (3,  2, INT 2), (3,  3, INT 3), (3,  4, INT 4)
	  ]
    val _ = List.app add [
	    (3, 0, 0, INT 0),   (3, 0, 1, INT 1),   (3, 0, 2, INT 2),   (3, 0, 3, INT 3),
	    (3, 0, 4, INT 4),   (3, 0, 5, INT 5),   (3, 0, 6, INT 6),   (3, 0, 7, INT 7),
	    (3, 1, 0, INT 1),   (3, 1, 1, INT 2),   (3, 1, 2, INT 3),   (3, 1, 3, INT 4),
	    (3, 1, 4, INT 5),   (3, 1, 5, INT 6),   (3, 1, 6, INT 7),   (3, 1, 7, OVFL_EXN),
	    (3, 2, 0, INT 2),   (3, 2, 1, INT 3),   (3, 2, 2, INT 4),   (3, 2, 3, INT 5),
	    (3, 2, 4, INT 6),   (3, 2, 5, INT 7),   (3, 2, 6, OVFL_EXN), (3, 2, 7, OVFL_EXN),
	    (3, 3, 0, INT 3),   (3, 3, 1, INT 4),   (3, 3, 2, INT 5),   (3, 3, 3, INT 6),
	    (3, 3, 4, INT 7),   (3, 3, 5, OVFL_EXN), (3, 3, 6, OVFL_EXN), (3, 3, 7, OVFL_EXN),
	    (3, 4, 0, INT 4),   (3, 4, 1, INT 5),   (3, 4, 2, INT 6),   (3, 4, 3, INT 7),
	    (3, 4, 4, OVFL_EXN), (3, 4, 5, OVFL_EXN), (3, 4, 6, OVFL_EXN), (3, 4, 7, OVFL_EXN),
	    (3, 5, 0, INT 5),   (3, 5, 1, INT 6),   (3, 5, 2, INT 7),   (3, 5, 3, OVFL_EXN),
	    (3, 5, 4, OVFL_EXN), (3, 5, 5, OVFL_EXN), (3, 5, 6, OVFL_EXN), (3, 5, 7, OVFL_EXN),
	    (3, 6, 0, INT 6),   (3, 6, 1, INT 7),   (3, 6, 2, OVFL_EXN), (3, 6, 3, OVFL_EXN),
	    (3, 6, 4, OVFL_EXN), (3, 6, 5, OVFL_EXN), (3, 6, 6, OVFL_EXN), (3, 6, 7, OVFL_EXN),
	    (3, 7, 0, INT 7),   (3, 7, 1, OVFL_EXN), (3, 7, 2, OVFL_EXN), (3, 7, 3, OVFL_EXN),
	    (3, 7, 4, OVFL_EXN), (3, 7, 5, OVFL_EXN), (3, 7, 6, OVFL_EXN), (3, 7, 7, OVFL_EXN)
          ]
    val _ = List.app sub [
	    (3, 0, 0, INT 0), (3, 0, 1, OVFL_EXN), (3, 0, 2, OVFL_EXN), (3, 0, 3, OVFL_EXN),
	    (3, 0, 4, OVFL_EXN), (3, 0, 5, OVFL_EXN), (3, 0, 6, OVFL_EXN), (3, 0, 7, OVFL_EXN),
	    (3, 1, 0, INT 1),   (3, 1, 1, INT 0),   (3, 1, 2, OVFL_EXN), (3, 1, 3,OVFL_EXN),
	    (3, 1, 4, OVFL_EXN), (3, 1, 5, OVFL_EXN), (3, 1, 6, OVFL_EXN), (3, 1, 7, OVFL_EXN),
	    (3, 2, 0, INT 2),   (3, 2, 1, INT 1),   (3, 2, 2, INT 0),   (3, 2, 3, OVFL_EXN),
	    (3, 2, 4, OVFL_EXN), (3, 2, 5, OVFL_EXN), (3, 2, 6, OVFL_EXN), (3, 2, 7, OVFL_EXN),
	    (3, 3, 0, INT 3),   (3, 3, 1, INT 2),   (3, 3, 2, INT 1),   (3, 3, 3, INT 0),
	    (3, 3, 4, OVFL_EXN), (3, 3, 5, OVFL_EXN), (3, 3, 6, OVFL_EXN), (3, 3, 7, OVFL_EXN),
	    (3, 4, 0, INT 4),   (3, 4, 1, INT 3),   (3, 4, 2, INT 2),   (3, 4, 3, INT 1),
	    (3, 4, 4, INT 0),   (3, 4, 5, OVFL_EXN), (3, 4, 6, OVFL_EXN),(3, 4, 7, OVFL_EXN),
	    (3, 5, 0, INT 5),   (3, 5, 1, INT 4),   (3, 5, 2, INT 3),   (3, 5, 3, INT 2),
	    (3, 5, 4, INT 1),   (3, 5, 5, INT 0),   (3, 5, 6, OVFL_EXN),(3, 5, 7, OVFL_EXN),
	    (3, 6, 0, INT 6),   (3, 6, 1, INT 5),   (3, 6, 2, INT 4),   (3, 6, 3, INT 3),
	    (3, 6, 4, INT 2),   (3, 6, 5, INT 1),   (3, 6, 6, INT 0),   (3, 6, 7, OVFL_EXN),
	    (3, 7, 0, INT 7),   (3, 7, 1, INT 6),   (3, 7, 2, INT 5),   (3, 7, 3, INT 4),
	    (3, 7, 4, INT 3),   (3, 7, 5, INT 2),   (3, 7, 6, INT 1),   (3, 7, 7, INT 0)
          ]
    val _ = List.app mul [
	    (3, 0, 0, INT 0),   (3, 0, 1, INT 0),   (3, 0, 2, INT 0),   (3, 0, 3, INT 0),
	    (3, 0, 4, INT 0),   (3, 0, 5, INT 0),   (3, 0, 6, INT 0),   (3, 0, 7, INT 0),
	    (3, 1, 0, INT 0),   (3, 1, 1, INT 1),   (3, 1, 2, INT 2),   (3, 1, 3, INT 3),
	    (3, 1, 4, INT 4),   (3, 1, 5, INT 5),   (3, 1, 6, INT 6),   (3, 1, 7, INT 7),
	    (3, 2, 0, INT 0),   (3, 2, 1, INT 2),   (3, 2, 2, INT 4),   (3, 2, 3, INT 6),
	    (3, 2, 4, OVFL_EXN), (3, 2, 5, OVFL_EXN), (3, 2, 6, OVFL_EXN), (3, 2, 7, OVFL_EXN),
	    (3, 3, 0, INT 0),   (3, 3, 1, INT 3),   (3, 3, 2, INT 6),   (3, 3, 3, OVFL_EXN),
	    (3, 3, 4, OVFL_EXN), (3, 3, 5, OVFL_EXN), (3, 3, 6, OVFL_EXN), (3, 3, 7, OVFL_EXN),
	    (3, 4, 0, INT 0),   (3, 4, 1, INT 4),   (3, 4, 2, OVFL_EXN), (3, 4, 3, OVFL_EXN),
	    (3, 4, 4, OVFL_EXN), (3, 4, 5, OVFL_EXN), (3, 4, 6, OVFL_EXN), (3, 4, 7, OVFL_EXN),
	    (3, 5, 0, INT 0),   (3, 5, 1, INT 5),   (3, 5, 2, OVFL_EXN), (3, 5, 3, OVFL_EXN),
	    (3, 5, 4, OVFL_EXN), (3, 5, 5, OVFL_EXN), (3, 5, 6, OVFL_EXN), (3, 5, 7, OVFL_EXN),
	    (3, 6, 0, INT 0),   (3, 6, 1, INT 6),   (3, 6, 2, OVFL_EXN), (3, 6, 3, OVFL_EXN),
	    (3, 6, 4, OVFL_EXN), (3, 6, 5, OVFL_EXN), (3, 6, 6, OVFL_EXN), (3, 6, 7, OVFL_EXN),
	    (3, 7, 0, INT 0),   (3, 7, 1, INT 7),   (3, 7, 2, OVFL_EXN),(3, 7, 3, OVFL_EXN),
	    (3, 7, 4, OVFL_EXN),(3, 7, 5, OVFL_EXN),(3, 7, 6, OVFL_EXN),(3, 7, 7, OVFL_EXN)
          ]
    val _ = List.app div' [
	    (3, 0, 0, DIV_EXN), (3, 0, 1, INT 0),   (3, 0, 2, INT 0),   (3, 0, 3, INT 0),
	    (3, 0, 4, INT 0),   (3, 0, 5, INT 0),   (3, 0, 6, INT 0),   (3, 0, 7, INT 0),
	    (3, 1, 0, DIV_EXN), (3, 1, 1, INT 1),   (3, 1, 2, INT 0),   (3, 1, 3, INT 0),
	    (3, 1, 4, INT 0),   (3, 1, 5, INT 0),   (3, 1, 6, INT 0),   (3, 1, 7, INT 0),
	    (3, 2, 0, DIV_EXN), (3, 2, 1, INT 2),   (3, 2, 2, INT 1),   (3, 2, 3, INT 0),
	    (3, 2, 4, INT 0),   (3, 2, 5, INT 0),   (3, 2, 6, INT 0),   (3, 2, 7, INT 0),
	    (3, 3, 0, DIV_EXN), (3, 3, 1, INT 3),   (3, 3, 2, INT 1),   (3, 3, 3, INT 1),
	    (3, 3, 4, INT 0),   (3, 3, 5, INT 0),   (3, 3, 6, INT 0),   (3, 3, 7, INT 0),
	    (3, 4, 0, DIV_EXN), (3, 4, 1, INT 4),   (3, 4, 2, INT 2),   (3, 4, 3, INT 1),
	    (3, 4, 4, INT 1),   (3, 4, 5, INT 0),   (3, 4, 6, INT 0),   (3, 4, 7, INT 0),
	    (3, 5, 0, DIV_EXN), (3, 5, 1, INT 5),   (3, 5, 2, INT 2),   (3, 5, 3, INT 1),
	    (3, 5, 4, INT 1),   (3, 5, 5, INT 1),   (3, 5, 6, INT 0),   (3, 5, 7, INT 0),
	    (3, 6, 0, DIV_EXN), (3, 6, 1, INT 6),   (3, 6, 2, INT 3),   (3, 6, 3, INT 2),
	    (3, 6, 4, INT 1),   (3, 6, 5, INT 1),   (3, 6, 6, INT 1),   (3, 6, 7, INT 0),
	    (3, 7, 0, DIV_EXN), (3, 7, 1, INT 7),   (3, 7, 2, INT 3),   (3, 7, 3, INT 2),
	    (3, 7, 4, INT 1),   (3, 7, 5, INT 1),   (3, 7, 6, INT 1),   (3, 7, 7, INT 1)
          ]
    val _ = List.app mod' [
	    (3, 0, 0, DIV_EXN), (3, 0, 1, INT 0),   (3, 0, 2, INT 0),   (3, 0, 3, INT 0),
	    (3, 0, 4, INT 0),   (3, 0, 5, INT 0),   (3, 0, 6, INT 0),   (3, 0, 7, INT 0),
	    (3, 1, 0, DIV_EXN), (3, 1, 1, INT 0),   (3, 1, 2, INT 1),   (3, 1, 3, INT 1),
	    (3, 1, 4, INT 1),   (3, 1, 5, INT 1),   (3, 1, 6, INT 1),   (3, 1, 7, INT 1),
	    (3, 2, 0, DIV_EXN), (3, 2, 1, INT 0),   (3, 2, 2, INT 0),   (3, 2, 3, INT 2),
	    (3, 2, 4, INT 2),   (3, 2, 5, INT 2),   (3, 2, 6, INT 2),   (3, 2, 7, INT 2),
	    (3, 3, 0, DIV_EXN), (3, 3, 1, INT 0),   (3, 3, 2, INT 1),   (3, 3, 3, INT 0),
	    (3, 3, 4, INT 3),   (3, 3, 5, INT 3),   (3, 3, 6, INT 3),   (3, 3, 7, INT 3),
	    (3, 4, 0, DIV_EXN), (3, 4, 1, INT 0),   (3, 4, 2, INT 0),   (3, 4, 3, INT 1),
	    (3, 4, 4, INT 0),   (3, 4, 5, INT 4),   (3, 4, 6, INT 4),   (3, 4, 7, INT 4),
	    (3, 5, 0, DIV_EXN), (3, 5, 1, INT 0),   (3, 5, 2, INT 1),   (3, 5, 3, INT 2),
	    (3, 5, 4, INT 1),   (3, 5, 5, INT 0),   (3, 5, 6, INT 5),   (3, 5, 7, INT 5),
	    (3, 6, 0, DIV_EXN), (3, 6, 1, INT 0),   (3, 6, 2, INT 0),   (3, 6, 3, INT 0),
	    (3, 6, 4, INT 2),   (3, 6, 5, INT 1),   (3, 6, 6, INT 0),   (3, 6, 7, INT 6),
	    (3, 7, 0, DIV_EXN), (3, 7, 1, INT 0),   (3, 7, 2, INT 1),   (3, 7, 3, INT 1),
	    (3, 7, 4, INT 3),   (3, 7, 5, INT 2),   (3, 7, 6, INT 1),   (3, 7, 7, INT 0)
          ]
    val _ = List.app neg [
	    (3, 0, INT 0), (3, 1, INT 7), (3, 2, INT 6), (3, 3, INT 5), (3, 4, INT 4),
	    (3, 5, INT 3), (3, 6, INT 2), (3, 7, INT 1)
	  ]
    end (* local *)
  end (* TestUnsignedTrapping *)

structure TestUnsignedWrapping =
  struct
    local
      datatype z = datatype TestUtil.value
      structure A = UnsignedWrappingArith
      val check1 = TestUtil.check1 "UnsignedWrappingArith"
      val check2 = TestUtil.check2 "UnsignedWrappingArith"
      val narrow = check1 "uNarrow" A.uNarrow
      val toUnsigned = check1 "toUnsigned" A.toUnsigned
      val add = check2 "+" A.uAdd
      val sub = check2 "-" A.uSub
      val mul = check2 "*" A.uMul
      val div' = check2 "div" A.uDiv
      val mod' = check2 "mod" A.uMod
      val neg = check1 "~" A.uNeg
    in
    val _ = List.app narrow [
	    (3, 0, INT 0), (3, 1, INT 1), (3, 2, INT 2), (3, 3, INT 3), (3, 4, INT 4),
	    (3, 5, INT 5), (3, 6, INT 6), (3, 7, INT 7), (3, 8, INT 0), (3, 9, INT 1)
	  ]
    val _ = List.app toUnsigned [
	    (3, ~5, INT 3), (3, ~4, INT 4), (3, ~3, INT 5), (3, ~2, INT 6), (3, ~1, INT 7),
	    (3,  0, INT 0), (3,  1, INT 1), (3,  2, INT 2), (3,  3, INT 3), (3,  4, INT 4)
	  ]
    val _ = List.app add [
	    (3, 0, 0, INT 0),   (3, 0, 1, INT 1),   (3, 0, 2, INT 2),   (3, 0, 3, INT 3),
	    (3, 0, 4, INT 4),   (3, 0, 5, INT 5),   (3, 0, 6, INT 6),   (3, 0, 7, INT 7),
	    (3, 1, 0, INT 1),   (3, 1, 1, INT 2),   (3, 1, 2, INT 3),   (3, 1, 3, INT 4),
	    (3, 1, 4, INT 5),   (3, 1, 5, INT 6),   (3, 1, 6, INT 7),   (3, 1, 7, INT 0),
	    (3, 2, 0, INT 2),   (3, 2, 1, INT 3),   (3, 2, 2, INT 4),   (3, 2, 3, INT 5),
	    (3, 2, 4, INT 6),   (3, 2, 5, INT 7),   (3, 2, 6, INT 0),   (3, 2, 7, INT 1),
	    (3, 3, 0, INT 3),   (3, 3, 1, INT 4),   (3, 3, 2, INT 5),   (3, 3, 3, INT 6),
	    (3, 3, 4, INT 7),   (3, 3, 5, INT 0),   (3, 3, 6, INT 1),   (3, 3, 7, INT 2),
	    (3, 4, 0, INT 4),   (3, 4, 1, INT 5),   (3, 4, 2, INT 6),   (3, 4, 3, INT 7),
	    (3, 4, 4, INT 0),   (3, 4, 5, INT 1),   (3, 4, 6, INT 2),   (3, 4, 7, INT 3),
	    (3, 5, 0, INT 5),   (3, 5, 1, INT 6),   (3, 5, 2, INT 7),   (3, 5, 3, INT 0),
	    (3, 5, 4, INT 1),   (3, 5, 5, INT 2),   (3, 5, 6, INT 3),   (3, 5, 7, INT 4),
	    (3, 6, 0, INT 6),   (3, 6, 1, INT 7),   (3, 6, 2, INT 0),   (3, 6, 3, INT 1),
	    (3, 6, 4, INT 2),   (3, 6, 5, INT 3),   (3, 6, 6, INT 4),   (3, 6, 7, INT 5),
	    (3, 7, 0, INT 7),   (3, 7, 1, INT 0),   (3, 7, 2, INT 1),   (3, 7, 3, INT 2),
	    (3, 7, 4, INT 3),   (3, 7, 5, INT 4),   (3, 7, 6, INT 5),   (3, 7, 7, INT 6)
          ]
    val _ = List.app sub [
	    (3, 0, 0, INT 0),   (3, 0, 1, INT 7),   (3, 0, 2, INT 6),   (3, 0, 3, INT 5),
	    (3, 0, 4, INT 4),   (3, 0, 5, INT 3),   (3, 0, 6, INT 2),   (3, 0, 7, INT 1),
	    (3, 1, 0, INT 1),   (3, 1, 1, INT 0),   (3, 1, 2, INT 7),   (3, 1, 3, INT 6),
	    (3, 1, 4, INT 5),   (3, 1, 5, INT 4),   (3, 1, 6, INT 3),   (3, 1, 7, INT 2),
	    (3, 2, 0, INT 2),   (3, 2, 1, INT 1),   (3, 2, 2, INT 0),   (3, 2, 3, INT 7),
	    (3, 2, 4, INT 6),   (3, 2, 5, INT 5),   (3, 2, 6, INT 4),   (3, 2, 7, INT 3),
	    (3, 3, 0, INT 3),   (3, 3, 1, INT 2),   (3, 3, 2, INT 1),   (3, 3, 3, INT 0),
	    (3, 3, 4, INT 7),   (3, 3, 5, INT 6),   (3, 3, 6, INT 5),   (3, 3, 7, INT 4),
	    (3, 4, 0, INT 4),   (3, 4, 1, INT 3),   (3, 4, 2, INT 2),   (3, 4, 3, INT 1),
	    (3, 4, 4, INT 0),   (3, 4, 5, INT 7),   (3, 4, 6, INT 6),   (3, 4, 7, INT 5),
	    (3, 5, 0, INT 5),   (3, 5, 1, INT 4),   (3, 5, 2, INT 3),   (3, 5, 3, INT 2),
	    (3, 5, 4, INT 1),   (3, 5, 5, INT 0),   (3, 5, 6, INT 7),   (3, 5, 7, INT 6),
	    (3, 6, 0, INT 6),   (3, 6, 1, INT 5),   (3, 6, 2, INT 4),   (3, 6, 3, INT 3),
	    (3, 6, 4, INT 2),   (3, 6, 5, INT 1),   (3, 6, 6, INT 0),   (3, 6, 7, INT 7),
	    (3, 7, 0, INT 7),   (3, 7, 1, INT 6),   (3, 7, 2, INT 5),   (3, 7, 3, INT 4),
	    (3, 7, 4, INT 3),   (3, 7, 5, INT 2),   (3, 7, 6, INT 1),   (3, 7, 7, INT 0)
          ]
    val _ = List.app mul [
	    (3, 0, 0, INT 0),   (3, 0, 1, INT 0),   (3, 0, 2, INT 0),   (3, 0, 3, INT 0),
	    (3, 0, 4, INT 0),   (3, 0, 5, INT 0),   (3, 0, 6, INT 0),   (3, 0, 7, INT 0),
	    (3, 1, 0, INT 0),   (3, 1, 1, INT 1),   (3, 1, 2, INT 2),   (3, 1, 3, INT 3),
	    (3, 1, 4, INT 4),   (3, 1, 5, INT 5),   (3, 1, 6, INT 6),   (3, 1, 7, INT 7),
	    (3, 2, 0, INT 0),   (3, 2, 1, INT 2),   (3, 2, 2, INT 4),   (3, 2, 3, INT 6),
	    (3, 2, 4, INT 0),   (3, 2, 5, INT 2),   (3, 2, 6, INT 4),   (3, 2, 7, INT 6),
	    (3, 3, 0, INT 0),   (3, 3, 1, INT 3),   (3, 3, 2, INT 6),   (3, 3, 3, INT 1),
	    (3, 3, 4, INT 4),   (3, 3, 5, INT 7),   (3, 3, 6, INT 2),   (3, 3, 7, INT 5),
	    (3, 4, 0, INT 0),   (3, 4, 1, INT 4),   (3, 4, 2, INT 0),   (3, 4, 3, INT 4),
	    (3, 4, 4, INT 0),   (3, 4, 5, INT 4),   (3, 4, 6, INT 0),   (3, 4, 7, INT 4),
	    (3, 5, 0, INT 0),   (3, 5, 1, INT 5),   (3, 5, 2, INT 2),   (3, 5, 3, INT 7),
	    (3, 5, 4, INT 4),   (3, 5, 5, INT 1),   (3, 5, 6, INT 6),   (3, 5, 7, INT 3),
	    (3, 6, 0, INT 0),   (3, 6, 1, INT 6),   (3, 6, 2, INT 4),   (3, 6, 3, INT 2),
	    (3, 6, 4, INT 0),   (3, 6, 5, INT 6),   (3, 6, 6, INT 4),   (3, 6, 7, INT 2),
	    (3, 7, 0, INT 0),   (3, 7, 1, INT 7),   (3, 7, 2, INT 6),   (3, 7, 3, INT 5),
	    (3, 7, 4, INT 4),   (3, 7, 5, INT 3),   (3, 7, 6, INT 2),   (3, 7, 7, INT 1)
          ]
    val _ = List.app div' [
	    (3, 0, 0, DIV_EXN), (3, 0, 1, INT 0),   (3, 0, 2, INT 0),   (3, 0, 3, INT 0),
	    (3, 0, 4, INT 0),   (3, 0, 5, INT 0),   (3, 0, 6, INT 0),   (3, 0, 7, INT 0),
	    (3, 1, 0, DIV_EXN), (3, 1, 1, INT 1),   (3, 1, 2, INT 0),   (3, 1, 3, INT 0),
	    (3, 1, 4, INT 0),   (3, 1, 5, INT 0),   (3, 1, 6, INT 0),   (3, 1, 7, INT 0),
	    (3, 2, 0, DIV_EXN), (3, 2, 1, INT 2),   (3, 2, 2, INT 1),   (3, 2, 3, INT 0),
	    (3, 2, 4, INT 0),   (3, 2, 5, INT 0),   (3, 2, 6, INT 0),   (3, 2, 7, INT 0),
	    (3, 3, 0, DIV_EXN), (3, 3, 1, INT 3),   (3, 3, 2, INT 1),   (3, 3, 3, INT 1),
	    (3, 3, 4, INT 0),   (3, 3, 5, INT 0),   (3, 3, 6, INT 0),   (3, 3, 7, INT 0),
	    (3, 4, 0, DIV_EXN), (3, 4, 1, INT 4),   (3, 4, 2, INT 2),   (3, 4, 3, INT 1),
	    (3, 4, 4, INT 1),   (3, 4, 5, INT 0),   (3, 4, 6, INT 0),   (3, 4, 7, INT 0),
	    (3, 5, 0, DIV_EXN), (3, 5, 1, INT 5),   (3, 5, 2, INT 2),   (3, 5, 3, INT 1),
	    (3, 5, 4, INT 1),   (3, 5, 5, INT 1),   (3, 5, 6, INT 0),   (3, 5, 7, INT 0),
	    (3, 6, 0, DIV_EXN), (3, 6, 1, INT 6),   (3, 6, 2, INT 3),   (3, 6, 3, INT 2),
	    (3, 6, 4, INT 1),   (3, 6, 5, INT 1),   (3, 6, 6, INT 1),   (3, 6, 7, INT 0),
	    (3, 7, 0, DIV_EXN), (3, 7, 1, INT 7),   (3, 7, 2, INT 3),   (3, 7, 3, INT 2),
	    (3, 7, 4, INT 1),   (3, 7, 5, INT 1),   (3, 7, 6, INT 1),   (3, 7, 7, INT 1)
          ]
    val _ = List.app mod' [
	    (3, 0, 0, DIV_EXN), (3, 0, 1, INT 0),   (3, 0, 2, INT 0),   (3, 0, 3, INT 0),
	    (3, 0, 4, INT 0),   (3, 0, 5, INT 0),   (3, 0, 6, INT 0),   (3, 0, 7, INT 0),
	    (3, 1, 0, DIV_EXN), (3, 1, 1, INT 0),   (3, 1, 2, INT 1),   (3, 1, 3, INT 1),
	    (3, 1, 4, INT 1),   (3, 1, 5, INT 1),   (3, 1, 6, INT 1),   (3, 1, 7, INT 1),
	    (3, 2, 0, DIV_EXN), (3, 2, 1, INT 0),   (3, 2, 2, INT 0),   (3, 2, 3, INT 2),
	    (3, 2, 4, INT 2),   (3, 2, 5, INT 2),   (3, 2, 6, INT 2),   (3, 2, 7, INT 2),
	    (3, 3, 0, DIV_EXN), (3, 3, 1, INT 0),   (3, 3, 2, INT 1),   (3, 3, 3, INT 0),
	    (3, 3, 4, INT 3),   (3, 3, 5, INT 3),   (3, 3, 6, INT 3),   (3, 3, 7, INT 3),
	    (3, 4, 0, DIV_EXN), (3, 4, 1, INT 0),   (3, 4, 2, INT 0),   (3, 4, 3, INT 1),
	    (3, 4, 4, INT 0),   (3, 4, 5, INT 4),   (3, 4, 6, INT 4),   (3, 4, 7, INT 4),
	    (3, 5, 0, DIV_EXN), (3, 5, 1, INT 0),   (3, 5, 2, INT 1),   (3, 5, 3, INT 2),
	    (3, 5, 4, INT 1),   (3, 5, 5, INT 0),   (3, 5, 6, INT 5),   (3, 5, 7, INT 5),
	    (3, 6, 0, DIV_EXN), (3, 6, 1, INT 0),   (3, 6, 2, INT 0),   (3, 6, 3, INT 0),
	    (3, 6, 4, INT 2),   (3, 6, 5, INT 1),   (3, 6, 6, INT 0),   (3, 6, 7, INT 6),
	    (3, 7, 0, DIV_EXN), (3, 7, 1, INT 0),   (3, 7, 2, INT 1),   (3, 7, 3, INT 1),
	    (3, 7, 4, INT 3),   (3, 7, 5, INT 2),   (3, 7, 6, INT 1),   (3, 7, 7, INT 0)
          ]
    val _ = List.app neg [
	    (3, 0, INT 0), (3, 1, INT 7), (3, 2, INT 6), (3, 3, INT 5), (3, 4, INT 4),
	    (3, 5, INT 3), (3, 6, INT 2), (3, 7, INT 1)
	  ]
    end (* local *)
  end (* TestUnsignedWrapping *)

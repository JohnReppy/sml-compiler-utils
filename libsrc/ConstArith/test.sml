(* test.sml
 *
 * Tests for the Constant Arithmetic library.
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

    fun checkApp f arg (expected : value) = let
	  val res = eval f arg
	  in
	    if (expected = res)
	      then OK
	      else BAD{expected = expected, actual = res}
	  end
	    handle ex => EXN ex

    fun eval' f arg = BOOL(f arg)
	  handle Overflow => OVFL_EXN
	       | Div => DIV_EXN

    fun checkApp' f arg (expected : value) = let
	  val res = eval' f arg
	  in
	    if (expected = res)
	      then OK
	      else BAD{expected = expected, actual = res}
	  end
	    handle ex => EXN ex

    fun pr msg = (
	  TextIO.output(TextIO.stdOut, concat msg);
	  TextIO.flushOut TextIO.stdOut)

    fun check1 strId w funId (f : int * IntInf.int -> IntInf.int) (arg, res) =
	  pr [
	      StringCvt.padRight #" " 31 (strId ^ ":"), " ",
	      StringCvt.padLeft #" " 10 (concat[funId, " ", IntInf.toString arg]),
	      " == ", valueToString res, " ", resultToString(checkApp f (w, arg) res), "\n"
	    ]

    fun check2 strId w funId (f : int * IntInf.int * IntInf.int -> IntInf.int) (arg1, arg2, res) =
	  pr [
	      StringCvt.padRight #" " 31 (strId ^ ":"), " ",
	      StringCvt.padLeft #" " 10 (concat[
		  IntInf.toString arg1, " ", funId, " ", IntInf.toString arg2
		]),
	      " == ", valueToString res, " ",
              resultToString(checkApp f (w, arg1, arg2) res), "\n"
	    ]

    fun checkCmp strId w funId (f : int * IntInf.int * IntInf.int -> bool) (arg1, arg2, res) =
	  pr [
	      StringCvt.padRight #" " 31 (strId ^ ":"), " ",
	      StringCvt.padLeft #" " 10 (concat[
		  IntInf.toString arg1, " ", funId, " ", IntInf.toString arg2
		]),
	      " == ", valueToString res, " ",
              resultToString(checkApp' f (w, arg1, arg2) res), "\n"
	    ]

  end; (* TestUtil *)

structure TestBitwiseArith =
  struct
    local
      datatype z = datatype TestUtil.value
      structure A = BitwiseConstArith
      val check1 = TestUtil.check1 "BitwiseConstArith" 3
      val check2 = TestUtil.check2 "BitwiseConstArith" 3
      val bAnd = check2 "AND" A.bAnd
      val bOr  = check2 "OR" A.bOr
      val bXor = check2 "XOR" A.bXor
      val bNot = check1 "NOT" A.bNot
      val bRotateL = check2 "ROTL" A.bRotateL
      val bRotateR = check2 "ROTR" A.bRotateR
      val bCountOnes = check1 "POPCNT" A.bCountOnes
      val bCountLeadingZeros = check1 "CLZ" A.bCountLeadingZeros
      val bCountTrailingZeros = check1 "CTZ" A.bCountTrailingZeros
    in
    fun test () = (
	  List.app bAnd [
	      (0, 0, INT 0),   (0, 1, INT 0),   (0, 2, INT 0),   (0, 3, INT 0),
	      (0, 4, INT 0),   (0, 5, INT 0),   (0, 6, INT 0),   (0, 7, INT 0),
	      (1, 0, INT 0),   (1, 1, INT 1),   (1, 2, INT 0),   (1, 3, INT 1),
	      (1, 4, INT 0),   (1, 5, INT 1),   (1, 6, INT 0),   (1, 7, INT 1),
	      (2, 0, INT 0),   (2, 1, INT 0),   (2, 2, INT 2),   (2, 3, INT 2),
	      (2, 4, INT 0),   (2, 5, INT 0),   (2, 6, INT 2),   (2, 7, INT 2),
	      (3, 0, INT 0),   (3, 1, INT 1),   (3, 2, INT 2),   (3, 3, INT 3),
	      (3, 4, INT 0),   (3, 5, INT 1),   (3, 6, INT 2),   (3, 7, INT 3),
	      (4, 0, INT 0),   (4, 1, INT 0),   (4, 2, INT 0),   (4, 3, INT 0),
	      (4, 4, INT 4),   (4, 5, INT 4),   (4, 6, INT 4),   (4, 7, INT 4),
	      (5, 0, INT 0),   (5, 1, INT 1),   (5, 2, INT 0),   (5, 3, INT 1),
	      (5, 4, INT 4),   (5, 5, INT 5),   (5, 6, INT 4),   (5, 7, INT 5),
	      (6, 0, INT 0),   (6, 1, INT 0),   (6, 2, INT 2),   (6, 3, INT 2),
	      (6, 4, INT 4),   (6, 5, INT 4),   (6, 6, INT 6),   (6, 7, INT 6),
	      (7, 0, INT 0),   (7, 1, INT 1),   (7, 2, INT 2),   (7, 3, INT 3),
	      (7, 4, INT 4),   (7, 5, INT 5),   (7, 6, INT 6),   (7, 7, INT 7)
	    ];
	  List.app bOr [
	      (0, 0, INT 0),   (0, 1, INT 1),   (0, 2, INT 2),   (0, 3, INT 3),
	      (0, 4, INT 4),   (0, 5, INT 5),   (0, 6, INT 6),   (0, 7, INT 7),
	      (1, 0, INT 1),   (1, 1, INT 1),   (1, 2, INT 3),   (1, 3, INT 3),
	      (1, 4, INT 5),   (1, 5, INT 5),   (1, 6, INT 7),   (1, 7, INT 7),
	      (2, 0, INT 2),   (2, 1, INT 3),   (2, 2, INT 2),   (2, 3, INT 3),
	      (2, 4, INT 6),   (2, 5, INT 7),   (2, 6, INT 6),   (2, 7, INT 7),
	      (3, 0, INT 3),   (3, 1, INT 3),   (3, 2, INT 3),   (3, 3, INT 3),
	      (3, 4, INT 7),   (3, 5, INT 7),   (3, 6, INT 7),   (3, 7, INT 7),
	      (4, 0, INT 4),   (4, 1, INT 5),   (4, 2, INT 6),   (4, 3, INT 7),
	      (4, 4, INT 4),   (4, 5, INT 5),   (4, 6, INT 6),   (4, 7, INT 7),
	      (5, 0, INT 5),   (5, 1, INT 5),   (5, 2, INT 7),   (5, 3, INT 7),
	      (5, 4, INT 5),   (5, 5, INT 5),   (5, 6, INT 7),   (5, 7, INT 7),
	      (6, 0, INT 6),   (6, 1, INT 7),   (6, 2, INT 6),   (6, 3, INT 7),
	      (6, 4, INT 6),   (6, 5, INT 7),   (6, 6, INT 6),   (6, 7, INT 7),
	      (7, 0, INT 7),   (7, 1, INT 7),   (7, 2, INT 7),   (7, 3, INT 7),
	      (7, 4, INT 7),   (7, 5, INT 7),   (7, 6, INT 7),   (7, 7, INT 7)
	    ];
	  List.app bXor [
	      (0, 0, INT 0),   (0, 1, INT 1),   (0, 2, INT 2),   (0, 3, INT 3),
	      (0, 4, INT 4),   (0, 5, INT 5),   (0, 6, INT 6),   (0, 7, INT 7),
	      (1, 0, INT 1),   (1, 1, INT 0),   (1, 2, INT 3),   (1, 3, INT 2),
	      (1, 4, INT 5),   (1, 5, INT 4),   (1, 6, INT 7),   (1, 7, INT 6),
	      (2, 0, INT 2),   (2, 1, INT 3),   (2, 2, INT 0),   (2, 3, INT 1),
	      (2, 4, INT 6),   (2, 5, INT 7),   (2, 6, INT 4),   (2, 7, INT 5),
	      (3, 0, INT 3),   (3, 1, INT 2),   (3, 2, INT 1),   (3, 3, INT 0),
	      (3, 4, INT 7),   (3, 5, INT 6),   (3, 6, INT 5),   (3, 7, INT 4),
	      (4, 0, INT 4),   (4, 1, INT 5),   (4, 2, INT 6),   (4, 3, INT 7),
	      (4, 4, INT 0),   (4, 5, INT 1),   (4, 6, INT 2),   (4, 7, INT 3),
	      (5, 0, INT 5),   (5, 1, INT 4),   (5, 2, INT 7),   (5, 3, INT 6),
	      (5, 4, INT 1),   (5, 5, INT 0),   (5, 6, INT 3),   (5, 7, INT 2),
	      (6, 0, INT 6),   (6, 1, INT 7),   (6, 2, INT 4),   (6, 3, INT 5),
	      (6, 4, INT 2),   (6, 5, INT 3),   (6, 6, INT 0),   (6, 7, INT 1),
	      (7, 0, INT 7),   (7, 1, INT 6),   (7, 2, INT 5),   (7, 3, INT 4),
	      (7, 4, INT 3),   (7, 5, INT 2),   (7, 6, INT 1),   (7, 7, INT 0)
	    ];
	  List.app bNot [
	      (0, INT 7),   (1, INT 6),   (2, INT 5),   (3, INT 4),
	      (4, INT 3),   (5, INT 2),   (6, INT 1),   (7, INT 0)
	    ];
          List.app bRotateL [
              (0, 0, INT 0),  (1, 0, INT 1),  (2, 0, INT 2), (3, 0, INT 3),
              (4, 0, INT 4),  (5, 0, INT 5),  (6, 0, INT 6), (7, 0, INT 7),
              (0, 1, INT 0),  (1, 1, INT 2),  (2, 1, INT 4), (3, 1, INT 6),
              (4, 1, INT 1),  (5, 1, INT 3),  (6, 1, INT 5), (7, 1, INT 7),
              (0, 2, INT 0),  (1, 2, INT 4),  (2, 2, INT 1), (3, 2, INT 5),
              (4, 2, INT 2),  (5, 2, INT 6),  (6, 2, INT 3), (7, 2, INT 7),
              (0, 3, INT 0),  (1, 3, INT 1),  (2, 3, INT 2), (3, 3, INT 3),
              (4, 3, INT 4),  (5, 3, INT 5),  (6, 3, INT 6), (7, 3, INT 7)
            ];
          List.app bRotateR [
              (0, 0, INT 0),  (1, 0, INT 1),  (2, 0, INT 2), (3, 0, INT 3),
              (4, 0, INT 4),  (5, 0, INT 5),  (6, 0, INT 6), (7, 0, INT 7),
              (0, 1, INT 0),  (1, 1, INT 4),  (2, 1, INT 1), (3, 1, INT 5),
              (4, 1, INT 2),  (5, 1, INT 6),  (6, 1, INT 3), (7, 1, INT 7),
              (0, 2, INT 0),  (1, 2, INT 2),  (2, 2, INT 4), (3, 2, INT 6),
              (4, 2, INT 1),  (5, 2, INT 3),  (6, 2, INT 5), (7, 2, INT 7),
              (0, 3, INT 0),  (1, 3, INT 1),  (2, 3, INT 2), (3, 3, INT 3),
              (4, 3, INT 4),  (5, 3, INT 5),  (6, 3, INT 6), (7, 3, INT 7)
            ];
          List.app bCountOnes [
	      (0, INT 0),   (1, INT 1),   (2, INT 1),   (3, INT 2),
	      (4, INT 1),   (5, INT 2),   (6, INT 2),   (7, INT 3)
            ];
          List.app bCountLeadingZeros [
	      (0, INT 3),   (1, INT 2),   (2, INT 1),   (3, INT 1),
	      (4, INT 0),   (5, INT 0),   (6, INT 0),   (7, INT 0)
            ];
          List.app bCountTrailingZeros [
	      (0, INT 3),   (1, INT 0),   (2, INT 1),   (3, INT 0),
	      (4, INT 2),   (5, INT 0),   (6, INT 1),   (7, INT 0)
            ];
          ())
    end (* local *)
  end (* TestBitwiseArith *)

structure TestSignedTrapping =
  struct
    local
      datatype z = datatype TestUtil.value
      structure A = SignedTrappingArith
      val check1 = TestUtil.check1 "SignedWrappingArith" 3
      val check2 = TestUtil.check2 "SignedWrappingArith" 3
      val narrow = check1 "sNarrow" A.sNarrow
      val toSigned = check1 "toSigned" A.toSigned
      val add = check2 "+" A.sAdd
      val sub = check2 "-" A.sSub
      val mul = check2 "*" A.sMul
      val div' = check2 "div" A.sDiv
      val mod' = check2 "mod" A.sMod
      val quot = check2 "quot" A.sQuot
      val rem = check2 "rem" A.sRem
      val sShL = check2 "sShL" A.sShL
      val sShR = check2 "sShR" A.sShR
      val neg = check1 "~" A.sNeg
      val abs = check1 "abs" A.sAbs
      val OVFLW = OVFL_EXN
      val DIVZ = DIV_EXN
    in
    fun test () = (
	  List.app narrow [
	      (~8, OVFLW),  (~7, OVFLW),  (~6, OVFLW),  (~5, OVFLW),
	      (~4, INT ~4), (~3, INT ~3), (~2, INT ~2), (~1, INT ~1),
	      ( 0, INT  0), ( 1, INT  1), ( 2, INT  2), ( 3, INT  3),
	      ( 4, OVFLW),  ( 5, OVFLW),  ( 6, OVFLW),  ( 7, OVFLW)
	    ];
	  List.app toSigned [
	      (0, INT  0), (1, INT  1), (2, INT  2), (3, INT  3),
	      (4, INT ~4), (5, INT ~3), (6, INT ~2), (7, INT ~1)
	    ];
          List.app add [
	      (~4, ~4, OVFLW),  (~4, ~3, OVFLW),  (~4, ~2, OVFLW),  (~4, ~1, OVFLW),
	      (~4,  0, INT ~4), (~4,  1, INT ~3), (~4,  2, INT ~2), (~4,  3, INT ~1),
	      (~3, ~4, OVFLW),  (~3, ~3, OVFLW),  (~3, ~2, OVFLW),  (~3, ~1, INT ~4),
	      (~3,  0, INT ~3), (~3,  1, INT ~2), (~3,  2, INT ~1), (~3,  3, INT  0),
	      (~2, ~4, OVFLW),  (~2, ~3, OVFLW),  (~2, ~2, INT ~4), (~2, ~1, INT ~3),
	      (~2,  0, INT ~2), (~2,  1, INT ~1), (~2,  2, INT  0), (~2,  3, INT  1),
	      (~1, ~4, OVFLW),  (~1, ~3, INT ~4), (~1, ~2, INT ~3), (~1, ~1, INT ~2),
	      (~1,  0, INT ~1), (~1,  1, INT  0), (~1,  2, INT  1), (~1,  3, INT  2),
	      ( 0, ~4, INT ~4), ( 0, ~3, INT ~3), ( 0, ~2, INT ~2), ( 0, ~1, INT ~1),
	      ( 0,  0, INT  0), ( 0,  1, INT  1), ( 0,  2, INT  2), ( 0,  3, INT  3),
	      ( 1, ~4, INT ~3), ( 1, ~3, INT ~2), ( 1, ~2, INT ~1), ( 1, ~1, INT  0),
	      ( 1,  0, INT  1), ( 1,  1, INT  2), ( 1,  2, INT  3), ( 1,  3, OVFLW),
	      ( 2, ~4, INT ~2), ( 2, ~3, INT ~1), ( 2, ~2, INT  0), ( 2, ~1, INT  1),
	      ( 2,  0, INT  2), ( 2,  1, INT  3), ( 2,  2, OVFLW),  ( 2,  3, OVFLW),
	      ( 3, ~4, INT ~1), ( 3, ~3, INT  0), ( 3, ~2, INT  1), ( 3, ~1, INT  2),
	      ( 3,  0, INT  3), ( 3,  1, OVFLW),  ( 3,  2, OVFLW),  ( 3,  3, OVFLW)
	    ];
          List.app sub [
	      (~4, ~4, INT  0), (~4, ~3, INT ~1), (~4, ~2, INT ~2), (~4, ~1, INT ~3),
	      (~4,  0, INT ~4), (~4,  1, OVFLW),  (~4,  2, OVFLW),  (~4,  3, OVFLW),
	      (~3, ~4, INT  1), (~3, ~3, INT  0), (~3, ~2, INT ~1), (~3, ~1, INT ~2),
	      (~3,  0, INT ~3), (~3,  1, INT ~4), (~3,  2, OVFLW),  (~3,  3, OVFLW),
	      (~2, ~4, INT  2), (~2, ~3, INT  1), (~2, ~2, INT  0), (~2, ~1, INT ~1),
	      (~2,  0, INT ~2), (~2,  1, INT ~3), (~2,  2, INT ~4), (~2,  3, OVFLW),
	      (~1, ~4, INT  3), (~1, ~3, INT  2), (~1, ~2, INT  1), (~1, ~1, INT  0),
	      (~1,  0, INT ~1), (~1,  1, INT ~2), (~1,  2, INT ~3), (~1,  3, INT ~4),
	      ( 0, ~4, OVFLW),  ( 0, ~3, INT  3), ( 0, ~2, INT  2), ( 0, ~1, INT  1),
	      ( 0,  0, INT  0), ( 0,  1, INT ~1), ( 0,  2, INT ~2), ( 0,  3, INT ~3),
	      ( 1, ~4, OVFLW),  ( 1, ~3, OVFLW),  ( 1, ~2, INT  3), ( 1, ~1, INT  2),
	      ( 1,  0, INT  1), ( 1,  1, INT  0), ( 1,  2, INT ~1), ( 1,  3, INT ~2),
	      ( 2, ~4, OVFLW),  ( 2, ~3, OVFLW),  ( 2, ~2, OVFLW),  ( 2, ~1, INT  3),
	      ( 2,  0, INT  2), ( 2,  1, INT  1), ( 2,  2, INT  0), ( 2,  3, INT ~1),
	      ( 3, ~4, OVFLW),  ( 3, ~3, OVFLW),  ( 3, ~2, OVFLW),  ( 3, ~1, OVFLW),
	      ( 3,  0, INT  3), ( 3,  1, INT  2), ( 3,  2, INT  1), ( 3,  3, INT  0)
	    ];
          List.app mul [
	      (~4, ~4, OVFLW),  (~4, ~3, OVFLW),  (~4, ~2, OVFLW),  (~4, ~1, OVFLW),
	      (~4,  0, INT  0), (~4,  1, INT ~4), (~4,  2, OVFLW),  (~4,  3, OVFLW),
	      (~3, ~4, OVFLW),  (~3, ~3, OVFLW),  (~3, ~2, OVFLW),  (~3, ~1, INT  3),
	      (~3,  0, INT  0), (~3,  1, INT ~3), (~3,  2, OVFLW),  (~3,  3, OVFLW),
	      (~2, ~4, OVFLW),  (~2, ~3, OVFLW),  (~2, ~2, OVFLW),  (~2, ~1, INT  2),
	      (~2,  0, INT  0), (~2,  1, INT ~2), (~2,  2, INT ~4), (~2,  3, OVFLW),
	      (~1, ~4, OVFLW),  (~1, ~3, INT  3), (~1, ~2, INT  2), (~1, ~1, INT  1),
	      (~1,  0, INT  0), (~1,  1, INT ~1), (~1,  2, INT ~2), (~1,  3, INT ~3),
	      ( 0, ~4, INT  0), ( 0, ~3, INT  0), ( 0, ~2, INT  0), ( 0, ~1, INT  0),
	      ( 0,  0, INT  0), ( 0,  1, INT  0), ( 0,  2, INT  0), ( 0,  3, INT  0),
	      ( 1, ~4, INT ~4), ( 1, ~3, INT ~3), ( 1, ~2, INT ~2), ( 1, ~1, INT ~1),
	      ( 1,  0, INT  0), ( 1,  1, INT  1), ( 1,  2, INT  2), ( 1,  3, INT  3),
	      ( 2, ~4, OVFLW),  ( 2, ~3, OVFLW),  ( 2, ~2, INT ~4), ( 2, ~1, INT ~2),
	      ( 2,  0, INT  0), ( 2,  1, INT  2), ( 2,  2, OVFLW),  ( 2,  3, OVFLW),
	      ( 3, ~4, OVFLW),  ( 3, ~3, OVFLW),  ( 3, ~2, OVFLW),  ( 3, ~1, INT ~3),
	      ( 3,  0, INT  0), ( 3,  1, INT  3), ( 3,  2, OVFLW),  ( 3,  3, OVFLW)
	    ];
          List.app div' [
	      (~4, ~4, INT 1),  (~4, ~3, INT 1),  (~4, ~2, INT 2),  (~4, ~1, OVFLW),
	      (~4,  0, DIVZ),   (~4,  1, INT ~4), (~4,  2, INT ~2), (~4,  3, INT ~2),
	      (~3, ~4, INT 0),  (~3, ~3, INT  1), (~3, ~2, INT  1), (~3, ~1, INT  3),
	      (~3,  0, DIVZ),   (~3,  1, INT ~3), (~3,  2, INT ~2), (~3,  3, INT ~1),
	      (~2, ~4, INT 0),  (~2, ~3, INT  0), (~2, ~2, INT  1), (~2, ~1, INT  2),
	      (~2,  0, DIVZ),   (~2,  1, INT ~2), (~2,  2, INT ~1), (~2,  3, INT ~1),
	      (~1, ~4, INT  0), (~1, ~3, INT  0), (~1, ~2, INT  0), (~1, ~1, INT  1),
	      (~1,  0, DIVZ),   (~1,  1, INT ~1), (~1,  2, INT ~1), (~1,  3, INT ~1),
	      ( 0, ~4, INT  0), ( 0, ~3, INT  0), ( 0, ~2, INT  0), ( 0, ~1, INT  0),
	      ( 0,  0, DIVZ),   ( 0,  1, INT  0), ( 0,  2, INT  0), ( 0,  3, INT  0),
	      ( 1, ~4, INT ~1), ( 1, ~3, INT ~1), ( 1, ~2, INT ~1), ( 1, ~1, INT ~1),
	      ( 1,  0, DIVZ),   ( 1,  1, INT  1), ( 1,  2, INT  0), ( 1,  3, INT  0),
	      ( 2, ~4, INT ~1), ( 2, ~3, INT ~1), ( 2, ~2, INT ~1), ( 2, ~1, INT ~2),
	      ( 2,  0, DIVZ),   ( 2,  1, INT  2), ( 2,  2, INT  1), ( 2,  3, INT  0),
	      ( 3, ~4, INT ~1), ( 3, ~3, INT ~1), ( 3, ~2, INT ~2), ( 3, ~1, INT ~3),
	      ( 3,  0, DIVZ),   ( 3,  1, INT  3), ( 3,  2, INT  1), ( 3,  3, INT  1)
	    ];
          List.app mod' [
	      (~4, ~4, INT 0),  (~4, ~3, INT ~1), (~4, ~2, INT 0),  (~4, ~1, INT  0),
	      (~4,  0, DIVZ),   (~4,  1, INT  0), (~4,  2, INT 0),  (~4,  3, INT  2),
	      (~3, ~4, INT ~3), (~3, ~3, INT  0), (~3, ~2, INT ~1), (~3, ~1, INT  0),
	      (~3,  0, DIVZ),   (~3,  1, INT  0), (~3,  2, INT  1), (~3,  3, INT  0),
	      (~2, ~4, INT ~2), (~2, ~3, INT ~2), (~2, ~2, INT  0), (~2, ~1, INT  0),
	      (~2,  0, DIVZ),   (~2,  1, INT  0), (~2,  2, INT  0), (~2,  3, INT  1),
	      (~1, ~4, INT ~1), (~1, ~3, INT ~1), (~1, ~2, INT ~1), (~1, ~1, INT  0),
	      (~1,  0, DIVZ),   (~1,  1, INT  0), (~1,  2, INT  1), (~1,  3, INT  2),
	      ( 0, ~4, INT  0), ( 0, ~3, INT  0), ( 0, ~2, INT  0), ( 0, ~1, INT  0),
	      ( 0,  0, DIVZ),   ( 0,  1, INT  0), ( 0,  2, INT  0), ( 0,  3, INT  0),
	      ( 1, ~4, INT ~3), ( 1, ~3, INT ~2), ( 1, ~2, INT ~1), ( 1, ~1, INT  0),
	      ( 1,  0, DIVZ),   ( 1,  1, INT  0), ( 1,  2, INT  1), ( 1,  3, INT  1),
	      ( 2, ~4, INT ~2), ( 2, ~3, INT ~1), ( 2, ~2, INT  0), ( 2, ~1, INT  0),
	      ( 2,  0, DIVZ),   ( 2,  1, INT  0), ( 2,  2, INT  0), ( 2,  3, INT  2),
	      ( 3, ~4, INT ~1), ( 3, ~3, INT  0), ( 3, ~2, INT ~1), ( 3, ~1, INT  0),
	      ( 3,  0, DIVZ),   ( 3,  1, INT  0), ( 3,  2, INT  1), ( 3,  3, INT  0)
	    ];
          List.app quot [
	      (~4, ~4, INT 1),  (~4, ~3, INT 1),  (~4, ~2, INT 2),  (~4, ~1, OVFLW),
	      (~4,  0, DIVZ),   (~4,  1, INT ~4), (~4,  2, INT ~2), (~4,  3, INT ~1),
	      (~3, ~4, INT 0),  (~3, ~3, INT  1), (~3, ~2, INT  1), (~3, ~1, INT  3),
	      (~3,  0, DIVZ),   (~3,  1, INT ~3), (~3,  2, INT ~1), (~3,  3, INT ~1),
	      (~2, ~4, INT 0),  (~2, ~3, INT  0), (~2, ~2, INT  1), (~2, ~1, INT  2),
	      (~2,  0, DIVZ),   (~2,  1, INT ~2), (~2,  2, INT ~1), (~2,  3, INT  0),
	      (~1, ~4, INT  0), (~1, ~3, INT  0), (~1, ~2, INT  0), (~1, ~1, INT  1),
	      (~1,  0, DIVZ),   (~1,  1, INT ~1), (~1,  2, INT  0), (~1,  3, INT  0),
	      ( 0, ~4, INT  0), ( 0, ~3, INT  0), ( 0, ~2, INT  0), ( 0, ~1, INT  0),
	      ( 0,  0, DIVZ),   ( 0,  1, INT  0), ( 0,  2, INT  0), ( 0,  3, INT  0),
	      ( 1, ~4, INT  0), ( 1, ~3, INT  0), ( 1, ~2, INT  0), ( 1, ~1, INT ~1),
	      ( 1,  0, DIVZ),   ( 1,  1, INT  1), ( 1,  2, INT  0), ( 1,  3, INT  0),
	      ( 2, ~4, INT  0), ( 2, ~3, INT  0), ( 2, ~2, INT ~1), ( 2, ~1, INT ~2),
	      ( 2,  0, DIVZ),   ( 2,  1, INT  2), ( 2,  2, INT  1), ( 2,  3, INT  0),
	      ( 3, ~4, INT  0), ( 3, ~3, INT ~1), ( 3, ~2, INT ~1), ( 3, ~1, INT ~3),
	      ( 3,  0, DIVZ),   ( 3,  1, INT  3), ( 3,  2, INT  1), ( 3,  3, INT  1)
	    ];
          List.app rem [
	      (~4, ~4, INT 0),  (~4, ~3, INT ~1), (~4, ~2, INT  0), (~4, ~1, INT  0),
	      (~4,  0, DIVZ),   (~4,  1, INT  0), (~4,  2, INT  0), (~4,  3, INT ~1),
	      (~3, ~4, INT ~3), (~3, ~3, INT  0), (~3, ~2, INT ~1), (~3, ~1, INT  0),
	      (~3,  0, DIVZ),   (~3,  1, INT  0), (~3,  2, INT ~1), (~3,  3, INT  0),
	      (~2, ~4, INT ~2), (~2, ~3, INT ~2), (~2, ~2, INT  0), (~2, ~1, INT  0),
	      (~2,  0, DIVZ),   (~2,  1, INT  0), (~2,  2, INT  0), (~2,  3, INT ~2),
	      (~1, ~4, INT ~1), (~1, ~3, INT ~1), (~1, ~2, INT ~1), (~1, ~1, INT  0),
	      (~1,  0, DIVZ),   (~1,  1, INT  0), (~1,  2, INT ~1), (~1,  3, INT ~1),
	      ( 0, ~4, INT  0), ( 0, ~3, INT  0), ( 0, ~2, INT  0), ( 0, ~1, INT  0),
	      ( 0,  0, DIVZ),   ( 0,  1, INT  0), ( 0,  2, INT  0), ( 0,  3, INT  0),
	      ( 1, ~4, INT  1), ( 1, ~3, INT  1), ( 1, ~2, INT  1), ( 1, ~1, INT  0),
	      ( 1,  0, DIVZ),   ( 1,  1, INT  0), ( 1,  2, INT  1), ( 1,  3, INT  1),
	      ( 2, ~4, INT  2), ( 2, ~3, INT  2), ( 2, ~2, INT  0), ( 2, ~1, INT  0),
	      ( 2,  0, DIVZ),   ( 2,  1, INT  0), ( 2,  2, INT  0), ( 2,  3, INT  2),
	      ( 3, ~4, INT  3), ( 3, ~3, INT  0), ( 3, ~2, INT  1), ( 3, ~1, INT  0),
	      ( 3,  0, DIVZ),   ( 3,  1, INT  0), ( 3,  2, INT  1), ( 3,  3, INT  0)
	    ];
          List.app sShL [
              (~4, 0, INT ~4),  (~4, 1, OVFLW),   (~4, 2, OVFLW),   (~4, 3, OVFLW),
              (~3, 0, INT ~3),  (~3, 1, OVFLW),   (~3, 2, OVFLW),   (~3, 3, OVFLW),
              (~2, 0, INT ~2),  (~2, 1, INT ~4),  (~2, 2, OVFLW),   (~2, 3, OVFLW),
              (~1, 0, INT ~1),  (~1, 1, INT ~2),  (~1, 2, INT ~4),  (~1, 3, OVFLW),
              ( 0, 0, INT  0),  ( 0, 1, INT  0),  ( 0, 2, INT  0),  ( 0, 3, INT  0),
              ( 1, 0, INT  1),  ( 1, 1, INT  2),  ( 1, 2, OVFLW),   ( 1, 3, OVFLW),
              ( 2, 0, INT  2),  ( 2, 1, OVFLW),   ( 2, 2, OVFLW),   ( 2, 3, OVFLW),
              ( 3, 0, INT  3),  ( 3, 1, OVFLW),   ( 3, 2, OVFLW),   ( 3, 3, OVFLW)
	    ];
          List.app sShR [
              (~4, 0, INT ~4),  (~4, 1, INT ~2),  (~4, 2, INT ~1),  (~4, 3, INT ~1),
              (~3, 0, INT ~3),  (~3, 1, INT ~2),  (~3, 2, INT ~1),  (~3, 3, INT ~1),
              (~2, 0, INT ~2),  (~2, 1, INT ~1),  (~2, 2, INT ~1),  (~2, 3, INT ~1),
              (~1, 0, INT ~1),  (~1, 1, INT ~1),  (~1, 2, INT ~1),  (~1, 3, INT ~1),
              ( 0, 0, INT  0),  ( 0, 1, INT  0),  ( 0, 2, INT  0),  ( 0, 3, INT  0),
              ( 1, 0, INT  1),  ( 1, 1, INT  0),  ( 1, 2, INT  0),  ( 1, 3, INT  0),
              ( 2, 0, INT  2),  ( 2, 1, INT  1),  ( 2, 2, INT  0),  ( 2, 3, INT  0),
              ( 3, 0, INT  3),  ( 3, 1, INT  1),  ( 3, 2, INT  0),  ( 3, 3, INT  0)
	    ];
	  List.app neg [
	      (~4, OVFLW),  (~3, INT  3), (~2, INT  2), (~1, INT  1),
	      ( 0, INT  0), ( 1, INT ~1), ( 2, INT ~2), ( 1, INT ~1)
	    ];
	  List.app abs [
	      (~4, OVFLW),  (~3, INT  3), (~2, INT  2), (~1, INT  1),
	      ( 0, INT  0), ( 1, INT  1), ( 2, INT  2), ( 1, INT  1)
	    ])
    end (* local *)
  end (* TestSignedTrapping *)

structure TestSignedWrapping =
  struct
    local
      datatype z = datatype TestUtil.value
      structure A = SignedWrappingArith
      val check1 = TestUtil.check1 "SignedWrappingArith" 3
      val check2 = TestUtil.check2 "SignedWrappingArith" 3
      val narrow = check1 "sNarrow" A.sNarrow
      val toSigned = check1 "toSigned" A.toSigned
      val add = check2 "+" A.sAdd
      val sub = check2 "-" A.sSub
      val mul = check2 "*" A.sMul
      val div' = check2 "div" A.sDiv
      val mod' = check2 "mod" A.sMod
      val quot = check2 "quot" A.sQuot
      val rem = check2 "rem" A.sRem
      val sShL = check2 "sShL" A.sShL
      val sShR = check2 "sShR" A.sShR
      val neg = check1 "~" A.sNeg
      val abs = check1 "abs" A.sAbs
      val DIVZ = DIV_EXN
    in
    fun test () = (
	  List.app narrow [
	      (~8, INT  0), (~7, INT  1), (~6, INT  2), (~5, INT  3),
	      (~4, INT ~4), (~3, INT ~3), (~2, INT ~2), (~1, INT ~1),
	      ( 0, INT  0), ( 1, INT  1), ( 2, INT  2), ( 3, INT  3),
	      ( 4, INT ~4), ( 5, INT ~3), ( 6, INT ~2), ( 7, INT ~1)
	    ];
	  List.app toSigned [
	      (0, INT  0), (1, INT  1), (2, INT  2), (3, INT  3),
	      (4, INT ~4), (5, INT ~3), (6, INT ~2), (7, INT ~1)
	    ];
          List.app add [
	      (~4, ~4, INT  0), (~4, ~3, INT  1), (~4, ~2, INT  2), (~4, ~1, INT  3),
	      (~4,  0, INT ~4), (~4,  1, INT ~3), (~4,  2, INT ~2), (~4,  3, INT ~1),
	      (~3, ~4, INT  1), (~3, ~3, INT  2), (~3, ~2, INT  3), (~3, ~1, INT ~4),
	      (~3,  0, INT ~3), (~3,  1, INT ~2), (~3,  2, INT ~1), (~3,  3, INT  0),
	      (~2, ~4, INT  2), (~2, ~3, INT  3), (~2, ~2, INT ~4), (~2, ~1, INT ~3),
	      (~2,  0, INT ~2), (~2,  1, INT ~1), (~2,  2, INT  0), (~2,  3, INT  1),
	      (~1, ~4, INT  3), (~1, ~3, INT ~4), (~1, ~2, INT ~3), (~1, ~1, INT ~2),
	      (~1,  0, INT ~1), (~1,  1, INT  0), (~1,  2, INT  1), (~1,  3, INT  2),
	      ( 0, ~4, INT ~4), ( 0, ~3, INT ~3), ( 0, ~2, INT ~2), ( 0, ~1, INT ~1),
	      ( 0,  0, INT  0), ( 0,  1, INT  1), ( 0,  2, INT  2), ( 0,  3, INT  3),
	      ( 1, ~4, INT ~3), ( 1, ~3, INT ~2), ( 1, ~2, INT ~1), ( 1, ~1, INT  0),
	      ( 1,  0, INT  1), ( 1,  1, INT  2), ( 1,  2, INT  3), ( 1,  3, INT ~4),
	      ( 2, ~4, INT ~2), ( 2, ~3, INT ~1), ( 2, ~2, INT  0), ( 2, ~1, INT  1),
	      ( 2,  0, INT  2), ( 2,  1, INT  3), ( 2,  2, INT ~4), ( 2,  3, INT ~3),
	      ( 3, ~4, INT ~1), ( 3, ~3, INT  0), ( 3, ~2, INT  1), ( 3, ~1, INT  2),
	      ( 3,  0, INT  3), ( 3,  1, INT ~4), ( 3,  2, INT ~3), ( 3,  3, INT ~2)
	    ];
          List.app sub [
	      (~4, ~4, INT  0), (~4, ~3, INT ~1), (~4, ~2, INT ~2), (~4, ~1, INT ~3),
	      (~4,  0, INT ~4), (~4,  1, INT  3), (~4,  2, INT  2), (~4,  3, INT  1),
	      (~3, ~4, INT  1), (~3, ~3, INT  0), (~3, ~2, INT ~1), (~3, ~1, INT ~2),
	      (~3,  0, INT ~3), (~3,  1, INT ~4), (~3,  2, INT  3), (~3,  3, INT  2),
	      (~2, ~4, INT  2), (~2, ~3, INT  1), (~2, ~2, INT  0), (~2, ~1, INT ~1),
	      (~2,  0, INT ~2), (~2,  1, INT ~3), (~2,  2, INT ~4), (~2,  3, INT  3),
	      (~1, ~4, INT  3), (~1, ~3, INT  2), (~1, ~2, INT  1), (~1, ~1, INT  0),
	      (~1,  0, INT ~1), (~1,  1, INT ~2), (~1,  2, INT ~3), (~1,  3, INT ~4),
	      ( 0, ~4, INT ~4), ( 0, ~3, INT  3), ( 0, ~2, INT  2), ( 0, ~1, INT  1),
	      ( 0,  0, INT  0), ( 0,  1, INT ~1), ( 0,  2, INT ~2), ( 0,  3, INT ~3),
	      ( 1, ~4, INT ~3), ( 1, ~3, INT ~4), ( 1, ~2, INT  3), ( 1, ~1, INT  2),
	      ( 1,  0, INT  1), ( 1,  1, INT  0), ( 1,  2, INT ~1), ( 1,  3, INT ~2),
	      ( 2, ~4, INT ~2), ( 2, ~3, INT ~3), ( 2, ~2, INT ~4), ( 2, ~1, INT  3),
	      ( 2,  0, INT  2), ( 2,  1, INT  1), ( 2,  2, INT  0), ( 2,  3, INT ~1),
	      ( 3, ~4, INT ~1), ( 3, ~3, INT ~2), ( 3, ~2, INT ~3), ( 3, ~1, INT ~4),
	      ( 3,  0, INT  3), ( 3,  1, INT  2), ( 3,  2, INT  1), ( 3,  3, INT  0)
	    ];
          List.app mul [
	      (~4, ~4, INT  0), (~4, ~3, INT ~4), (~4, ~2, INT  0), (~4, ~1, INT ~4),
	      (~4,  0, INT  0), (~4,  1, INT ~4), (~4,  2, INT  0), (~4,  3, INT ~4),
	      (~3, ~4, INT ~4), (~3, ~3, INT  1), (~3, ~2, INT ~2), (~3, ~1, INT  3),
	      (~3,  0, INT  0), (~3,  1, INT ~3), (~3,  2, INT  2), (~3,  3, INT ~1),
	      (~2, ~4, INT  0), (~2, ~3, INT ~2), (~2, ~2, INT ~4), (~2, ~1, INT  2),
	      (~2,  0, INT  0), (~2,  1, INT ~2), (~2,  2, INT ~4), (~2,  3, INT  2),
	      (~1, ~4, INT ~4), (~1, ~3, INT  3), (~1, ~2, INT  2), (~1, ~1, INT  1),
	      (~1,  0, INT  0), (~1,  1, INT ~1), (~1,  2, INT ~2), (~1,  3, INT ~3),
	      ( 0, ~4, INT  0), ( 0, ~3, INT  0), ( 0, ~2, INT  0), ( 0, ~1, INT  0),
	      ( 0,  0, INT  0), ( 0,  1, INT  0), ( 0,  2, INT  0), ( 0,  3, INT  0),
	      ( 1, ~4, INT ~4), ( 1, ~3, INT ~3), ( 1, ~2, INT ~2), ( 1, ~1, INT ~1),
	      ( 1,  0, INT  0), ( 1,  1, INT  1), ( 1,  2, INT  2), ( 1,  3, INT  3),
	      ( 2, ~4, INT  0), ( 2, ~3, INT  2), ( 2, ~2, INT ~4), ( 2, ~1, INT ~2),
	      ( 2,  0, INT  0), ( 2,  1, INT  2), ( 2,  2, INT ~4), ( 2,  3, INT ~2),
	      ( 3, ~4, INT ~4), ( 3, ~3, INT ~1), ( 3, ~2, INT  2), ( 3, ~1, INT ~3),
	      ( 3,  0, INT  0), ( 3,  1, INT  3), ( 3,  2, INT ~2), ( 3,  3, INT  1)
	    ];
          List.app div' [
	      (~4, ~4, INT 1),  (~4, ~3, INT 1),  (~4, ~2, INT 2),  (~4, ~1, INT ~4),
	      (~4,  0, DIVZ),   (~4,  1, INT ~4), (~4,  2, INT ~2), (~4,  3, INT ~2),
	      (~3, ~4, INT 0),  (~3, ~3, INT  1), (~3, ~2, INT  1), (~3, ~1, INT  3),
	      (~3,  0, DIVZ),   (~3,  1, INT ~3), (~3,  2, INT ~2), (~3,  3, INT ~1),
	      (~2, ~4, INT 0),  (~2, ~3, INT  0), (~2, ~2, INT  1), (~2, ~1, INT  2),
	      (~2,  0, DIVZ),   (~2,  1, INT ~2), (~2,  2, INT ~1), (~2,  3, INT ~1),
	      (~1, ~4, INT  0), (~1, ~3, INT  0), (~1, ~2, INT  0), (~1, ~1, INT  1),
	      (~1,  0, DIVZ),   (~1,  1, INT ~1), (~1,  2, INT ~1), (~1,  3, INT ~1),
	      ( 0, ~4, INT  0), ( 0, ~3, INT  0), ( 0, ~2, INT  0), ( 0, ~1, INT  0),
	      ( 0,  0, DIVZ),   ( 0,  1, INT  0), ( 0,  2, INT  0), ( 0,  3, INT  0),
	      ( 1, ~4, INT ~1), ( 1, ~3, INT ~1), ( 1, ~2, INT ~1), ( 1, ~1, INT ~1),
	      ( 1,  0, DIVZ),   ( 1,  1, INT  1), ( 1,  2, INT  0), ( 1,  3, INT  0),
	      ( 2, ~4, INT ~1), ( 2, ~3, INT ~1), ( 2, ~2, INT ~1), ( 2, ~1, INT ~2),
	      ( 2,  0, DIVZ),   ( 2,  1, INT  2), ( 2,  2, INT  1), ( 2,  3, INT  0),
	      ( 3, ~4, INT ~1), ( 3, ~3, INT ~1), ( 3, ~2, INT ~2), ( 3, ~1, INT ~3),
	      ( 3,  0, DIVZ),   ( 3,  1, INT  3), ( 3,  2, INT  1), ( 3,  3, INT  1)
	    ];
          List.app mod' [
	      (~4, ~4, INT 0),  (~4, ~3, INT ~1), (~4, ~2, INT 0),  (~4, ~1, INT  0),
	      (~4,  0, DIVZ),   (~4,  1, INT  0), (~4,  2, INT 0),  (~4,  3, INT  2),
	      (~3, ~4, INT ~3), (~3, ~3, INT  0), (~3, ~2, INT ~1), (~3, ~1, INT  0),
	      (~3,  0, DIVZ),   (~3,  1, INT  0), (~3,  2, INT  1), (~3,  3, INT  0),
	      (~2, ~4, INT ~2), (~2, ~3, INT ~2), (~2, ~2, INT  0), (~2, ~1, INT  0),
	      (~2,  0, DIVZ),   (~2,  1, INT  0), (~2,  2, INT  0), (~2,  3, INT  1),
	      (~1, ~4, INT ~1), (~1, ~3, INT ~1), (~1, ~2, INT ~1), (~1, ~1, INT  0),
	      (~1,  0, DIVZ),   (~1,  1, INT  0), (~1,  2, INT  1), (~1,  3, INT  2),
	      ( 0, ~4, INT  0), ( 0, ~3, INT  0), ( 0, ~2, INT  0), ( 0, ~1, INT  0),
	      ( 0,  0, DIVZ),   ( 0,  1, INT  0), ( 0,  2, INT  0), ( 0,  3, INT  0),
	      ( 1, ~4, INT ~3), ( 1, ~3, INT ~2), ( 1, ~2, INT ~1), ( 1, ~1, INT  0),
	      ( 1,  0, DIVZ),   ( 1,  1, INT  0), ( 1,  2, INT  1), ( 1,  3, INT  1),
	      ( 2, ~4, INT ~2), ( 2, ~3, INT ~1), ( 2, ~2, INT  0), ( 2, ~1, INT  0),
	      ( 2,  0, DIVZ),   ( 2,  1, INT  0), ( 2,  2, INT  0), ( 2,  3, INT  2),
	      ( 3, ~4, INT ~1), ( 3, ~3, INT  0), ( 3, ~2, INT ~1), ( 3, ~1, INT  0),
	      ( 3,  0, DIVZ),   ( 3,  1, INT  0), ( 3,  2, INT  1), ( 3,  3, INT  0)
	    ];
          List.app quot [
	      (~4, ~4, INT 1),  (~4, ~3, INT 1),  (~4, ~2, INT 2),  (~4, ~1, INT ~4),
	      (~4,  0, DIVZ),   (~4,  1, INT ~4), (~4,  2, INT ~2), (~4,  3, INT ~1),
	      (~3, ~4, INT 0),  (~3, ~3, INT  1), (~3, ~2, INT  1), (~3, ~1, INT  3),
	      (~3,  0, DIVZ),   (~3,  1, INT ~3), (~3,  2, INT ~1), (~3,  3, INT ~1),
	      (~2, ~4, INT 0),  (~2, ~3, INT  0), (~2, ~2, INT  1), (~2, ~1, INT  2),
	      (~2,  0, DIVZ),   (~2,  1, INT ~2), (~2,  2, INT ~1), (~2,  3, INT  0),
	      (~1, ~4, INT  0), (~1, ~3, INT  0), (~1, ~2, INT  0), (~1, ~1, INT  1),
	      (~1,  0, DIVZ),   (~1,  1, INT ~1), (~1,  2, INT  0), (~1,  3, INT  0),
	      ( 0, ~4, INT  0), ( 0, ~3, INT  0), ( 0, ~2, INT  0), ( 0, ~1, INT  0),
	      ( 0,  0, DIVZ),   ( 0,  1, INT  0), ( 0,  2, INT  0), ( 0,  3, INT  0),
	      ( 1, ~4, INT  0), ( 1, ~3, INT  0), ( 1, ~2, INT  0), ( 1, ~1, INT ~1),
	      ( 1,  0, DIVZ),   ( 1,  1, INT  1), ( 1,  2, INT  0), ( 1,  3, INT  0),
	      ( 2, ~4, INT  0), ( 2, ~3, INT  0), ( 2, ~2, INT ~1), ( 2, ~1, INT ~2),
	      ( 2,  0, DIVZ),   ( 2,  1, INT  2), ( 2,  2, INT  1), ( 2,  3, INT  0),
	      ( 3, ~4, INT  0), ( 3, ~3, INT ~1), ( 3, ~2, INT ~1), ( 3, ~1, INT ~3),
	      ( 3,  0, DIVZ),   ( 3,  1, INT  3), ( 3,  2, INT  1), ( 3,  3, INT  1)
	    ];
          List.app rem [
	      (~4, ~4, INT 0),  (~4, ~3, INT ~1), (~4, ~2, INT  0), (~4, ~1, INT  0),
	      (~4,  0, DIVZ),   (~4,  1, INT  0), (~4,  2, INT  0), (~4,  3, INT ~1),
	      (~3, ~4, INT ~3), (~3, ~3, INT  0), (~3, ~2, INT ~1), (~3, ~1, INT  0),
	      (~3,  0, DIVZ),   (~3,  1, INT  0), (~3,  2, INT ~1), (~3,  3, INT  0),
	      (~2, ~4, INT ~2), (~2, ~3, INT ~2), (~2, ~2, INT  0), (~2, ~1, INT  0),
	      (~2,  0, DIVZ),   (~2,  1, INT  0), (~2,  2, INT  0), (~2,  3, INT ~2),
	      (~1, ~4, INT ~1), (~1, ~3, INT ~1), (~1, ~2, INT ~1), (~1, ~1, INT  0),
	      (~1,  0, DIVZ),   (~1,  1, INT  0), (~1,  2, INT ~1), (~1,  3, INT ~1),
	      ( 0, ~4, INT  0), ( 0, ~3, INT  0), ( 0, ~2, INT  0), ( 0, ~1, INT  0),
	      ( 0,  0, DIVZ),   ( 0,  1, INT  0), ( 0,  2, INT  0), ( 0,  3, INT  0),
	      ( 1, ~4, INT  1), ( 1, ~3, INT  1), ( 1, ~2, INT  1), ( 1, ~1, INT  0),
	      ( 1,  0, DIVZ),   ( 1,  1, INT  0), ( 1,  2, INT  1), ( 1,  3, INT  1),
	      ( 2, ~4, INT  2), ( 2, ~3, INT  2), ( 2, ~2, INT  0), ( 2, ~1, INT  0),
	      ( 2,  0, DIVZ),   ( 2,  1, INT  0), ( 2,  2, INT  0), ( 2,  3, INT  2),
	      ( 3, ~4, INT  3), ( 3, ~3, INT  0), ( 3, ~2, INT  1), ( 3, ~1, INT  0),
	      ( 3,  0, DIVZ),   ( 3,  1, INT  0), ( 3,  2, INT  1), ( 3,  3, INT  0)
	    ];
          List.app sShL [
              (~4, 0, INT ~4),  (~4, 1, INT  0),  (~4, 2, INT  0),  (~4, 3, INT  0),
              (~3, 0, INT ~3),  (~3, 1, INT  2),  (~3, 2, INT ~4),  (~3, 3, INT  0),
              (~2, 0, INT ~2),  (~2, 1, INT ~4),  (~2, 2, INT  0),  (~2, 3, INT  0),
              (~1, 0, INT ~1),  (~1, 1, INT ~2),  (~1, 2, INT ~4),  (~1, 3, INT  0),
              ( 0, 0, INT  0),  ( 0, 1, INT  0),  ( 0, 2, INT  0),  ( 0, 3, INT  0),
              ( 1, 0, INT  1),  ( 1, 1, INT  2),  ( 1, 2, INT ~4),  ( 1, 3, INT  0),
              ( 2, 0, INT  2),  ( 2, 1, INT ~4),  ( 2, 2, INT  0),  ( 2, 3, INT  0),
              ( 3, 0, INT  3),  ( 3, 1, INT ~2),  ( 3, 2, INT ~4),  ( 3, 3, INT  0)
	    ];
          List.app sShR [
              (~4, 0, INT ~4),  (~4, 1, INT ~2),  (~4, 2, INT ~1),  (~4, 3, INT ~1),
              (~3, 0, INT ~3),  (~3, 1, INT ~2),  (~3, 2, INT ~1),  (~3, 3, INT ~1),
              (~2, 0, INT ~2),  (~2, 1, INT ~1),  (~2, 2, INT ~1),  (~2, 3, INT ~1),
              (~1, 0, INT ~1),  (~1, 1, INT ~1),  (~1, 2, INT ~1),  (~1, 3, INT ~1),
              ( 0, 0, INT  0),  ( 0, 1, INT  0),  ( 0, 2, INT  0),  ( 0, 3, INT  0),
              ( 1, 0, INT  1),  ( 1, 1, INT  0),  ( 1, 2, INT  0),  ( 1, 3, INT  0),
              ( 2, 0, INT  2),  ( 2, 1, INT  1),  ( 2, 2, INT  0),  ( 2, 3, INT  0),
              ( 3, 0, INT  3),  ( 3, 1, INT  1),  ( 3, 2, INT  0),  ( 3, 3, INT  0)
	    ];
	  List.app neg [
	      (~4, INT ~4),  (~3, INT  3), (~2, INT  2), (~1, INT  1),
	      ( 0, INT  0), ( 1, INT ~1), ( 2, INT ~2), ( 1, INT ~1)
	    ];
	  List.app abs [
	      (~4, INT ~4), (~3, INT  3), (~2, INT  2), (~1, INT  1),
	      ( 0, INT  0), ( 1, INT  1), ( 2, INT  2), ( 1, INT  1)
	    ])
    end (* local *)
  end (* TestSignedWrapping *)

structure TestUnsignedTrapping =
  struct
    local
      datatype z = datatype TestUtil.value
      structure A = UnsignedTrappingArith
      val check1 = TestUtil.check1 "UnsignedTrappingArith" 3
      val check2 = TestUtil.check2 "UnsignedTrappingArith" 3
      val checkCmp = TestUtil.checkCmp "UnsignedTrappingArith" 3
      val narrow = check1 "uNarrow" A.uNarrow
      val toUnsigned = check1 "toUnsigned" A.toUnsigned
      val add = check2 "+" A.uAdd
      val sub = check2 "-" A.uSub
      val mul = check2 "*" A.uMul
      val div' = check2 "div" A.uDiv
      val mod' = check2 "mod" A.uMod
      val uShL = check2 "uShL" A.uShL
      val uShR = check2 "uShR" A.uShR
      val neg = check1 "~" A.uNeg
      val eq = checkCmp "=" A.uEq
      val less = checkCmp "<" A.uLess
      val lessEq = checkCmp "<=" A.uLessEq
      val OVFLW = OVFL_EXN
      val DIVZ = DIV_EXN
      val TRUE = BOOL true
      val FALS = BOOL false
    in
    fun test () = (
	  List.app narrow [
	      (0, INT 0), (1, INT 1), (2, INT 2), (3, INT 3), (4, INT 4),
	      (5, INT 5), (6, INT 6), (7, INT 7), (8, OVFL_EXN), (9, OVFL_EXN)
	    ];
	  List.app toUnsigned [
	      (~5, INT 3), (~4, INT 4), (~3, INT 5), (~2, INT 6), (~1, INT 7),
	      ( 0, INT 0), ( 1, INT 1), ( 2, INT 2), ( 3, INT 3), ( 4, INT 4)
	    ];
	  List.app add [
	      (0, 0, INT 0),   (0, 1, INT 1),   (0, 2, INT 2),   (0, 3, INT 3),
	      (0, 4, INT 4),   (0, 5, INT 5),   (0, 6, INT 6),   (0, 7, INT 7),
	      (1, 0, INT 1),   (1, 1, INT 2),   (1, 2, INT 3),   (1, 3, INT 4),
	      (1, 4, INT 5),   (1, 5, INT 6),   (1, 6, INT 7),   (1, 7, OVFLW),
	      (2, 0, INT 2),   (2, 1, INT 3),   (2, 2, INT 4),   (2, 3, INT 5),
	      (2, 4, INT 6),   (2, 5, INT 7),   (2, 6, OVFLW),   (2, 7, OVFLW),
	      (3, 0, INT 3),   (3, 1, INT 4),   (3, 2, INT 5),   (3, 3, INT 6),
	      (3, 4, INT 7),   (3, 5, OVFLW),   (3, 6, OVFLW),   (3, 7, OVFLW),
	      (4, 0, INT 4),   (4, 1, INT 5),   (4, 2, INT 6),   (4, 3, INT 7),
	      (4, 4, OVFLW),   (4, 5, OVFLW),   (4, 6, OVFLW),   (4, 7, OVFLW),
	      (5, 0, INT 5),   (5, 1, INT 6),   (5, 2, INT 7),   (5, 3, OVFLW),
	      (5, 4, OVFLW),   (5, 5, OVFLW),   (5, 6, OVFLW),   (5, 7, OVFLW),
	      (6, 0, INT 6),   (6, 1, INT 7),   (6, 2, OVFLW),   (6, 3, OVFLW),
	      (6, 4, OVFLW),   (6, 5, OVFLW),   (6, 6, OVFLW),   (6, 7, OVFLW),
	      (7, 0, INT 7),   (7, 1, OVFLW),   (7, 2, OVFLW),   (7, 3, OVFLW),
	      (7, 4, OVFLW),   (7, 5, OVFLW),   (7, 6, OVFLW),   (7, 7, OVFLW)
	    ];
	  List.app sub [
	      (0, 0, INT 0),   (0, 1, OVFLW),   (0, 2, OVFLW),   (0, 3, OVFLW),
	      (0, 4, OVFLW),   (0, 5, OVFLW),   (0, 6, OVFLW),   (0, 7, OVFLW),
	      (1, 0, INT 1),   (1, 1, INT 0),   (1, 2, OVFLW),   (1, 3,OVFLW),
	      (1, 4, OVFLW),   (1, 5, OVFLW),   (1, 6, OVFLW),   (1, 7, OVFLW),
	      (2, 0, INT 2),   (2, 1, INT 1),   (2, 2, INT 0),   (2, 3, OVFLW),
	      (2, 4, OVFLW),   (2, 5, OVFLW),   (2, 6, OVFLW),   (2, 7, OVFLW),
	      (3, 0, INT 3),   (3, 1, INT 2),   (3, 2, INT 1),   (3, 3, INT 0),
	      (3, 4, OVFLW),   (3, 5, OVFLW),   (3, 6, OVFLW),   (3, 7, OVFLW),
	      (4, 0, INT 4),   (4, 1, INT 3),   (4, 2, INT 2),   (4, 3, INT 1),
	      (4, 4, INT 0),   (4, 5, OVFLW),   (4, 6, OVFLW),   (4, 7, OVFLW),
	      (5, 0, INT 5),   (5, 1, INT 4),   (5, 2, INT 3),   (5, 3, INT 2),
	      (5, 4, INT 1),   (5, 5, INT 0),   (5, 6, OVFLW),   (5, 7, OVFLW),
	      (6, 0, INT 6),   (6, 1, INT 5),   (6, 2, INT 4),   (6, 3, INT 3),
	      (6, 4, INT 2),   (6, 5, INT 1),   (6, 6, INT 0),   (6, 7, OVFLW),
	      (7, 0, INT 7),   (7, 1, INT 6),   (7, 2, INT 5),   (7, 3, INT 4),
	      (7, 4, INT 3),   (7, 5, INT 2),   (7, 6, INT 1),   (7, 7, INT 0)
	    ];
	  List.app mul [
	      (0, 0, INT 0),   (0, 1, INT 0),   (0, 2, INT 0),   (0, 3, INT 0),
	      (0, 4, INT 0),   (0, 5, INT 0),   (0, 6, INT 0),   (0, 7, INT 0),
	      (1, 0, INT 0),   (1, 1, INT 1),   (1, 2, INT 2),   (1, 3, INT 3),
	      (1, 4, INT 4),   (1, 5, INT 5),   (1, 6, INT 6),   (1, 7, INT 7),
	      (2, 0, INT 0),   (2, 1, INT 2),   (2, 2, INT 4),   (2, 3, INT 6),
	      (2, 4, OVFLW),   (2, 5, OVFLW),   (2, 6, OVFLW),   (2, 7, OVFLW),
	      (3, 0, INT 0),   (3, 1, INT 3),   (3, 2, INT 6),   (3, 3, OVFLW),
	      (3, 4, OVFLW),   (3, 5, OVFLW),   (3, 6, OVFLW),   (3, 7, OVFLW),
	      (4, 0, INT 0),   (4, 1, INT 4),   (4, 2, OVFLW),   (4, 3, OVFLW),
	      (4, 4, OVFLW),   (4, 5, OVFLW),   (4, 6, OVFLW),   (4, 7, OVFLW),
	      (5, 0, INT 0),   (5, 1, INT 5),   (5, 2, OVFLW),   (5, 3, OVFLW),
	      (5, 4, OVFLW),   (5, 5, OVFLW),   (5, 6, OVFLW),   (5, 7, OVFLW),
	      (6, 0, INT 0),   (6, 1, INT 6),   (6, 2, OVFLW),   (6, 3, OVFLW),
	      (6, 4, OVFLW),   (6, 5, OVFLW),   (6, 6, OVFLW),   (6, 7, OVFLW),
	      (7, 0, INT 0),   (7, 1, INT 7),   (7, 2, OVFLW),   (7, 3, OVFLW),
	      (7, 4, OVFLW),   (7, 5, OVFLW),   (7, 6, OVFLW),   (7, 7, OVFLW)
	    ];
	  List.app div' [
	      (0, 0, DIVZ),    (0, 1, INT 0),   (0, 2, INT 0),   (0, 3, INT 0),
	      (0, 4, INT 0),   (0, 5, INT 0),   (0, 6, INT 0),   (0, 7, INT 0),
	      (1, 0, DIVZ),    (1, 1, INT 1),   (1, 2, INT 0),   (1, 3, INT 0),
	      (1, 4, INT 0),   (1, 5, INT 0),   (1, 6, INT 0),   (1, 7, INT 0),
	      (2, 0, DIVZ),    (2, 1, INT 2),   (2, 2, INT 1),   (2, 3, INT 0),
	      (2, 4, INT 0),   (2, 5, INT 0),   (2, 6, INT 0),   (2, 7, INT 0),
	      (3, 0, DIVZ),    (3, 1, INT 3),   (3, 2, INT 1),   (3, 3, INT 1),
	      (3, 4, INT 0),   (3, 5, INT 0),   (3, 6, INT 0),   (3, 7, INT 0),
	      (4, 0, DIVZ),    (4, 1, INT 4),   (4, 2, INT 2),   (4, 3, INT 1),
	      (4, 4, INT 1),   (4, 5, INT 0),   (4, 6, INT 0),   (4, 7, INT 0),
	      (5, 0, DIVZ),    (5, 1, INT 5),   (5, 2, INT 2),   (5, 3, INT 1),
	      (5, 4, INT 1),   (5, 5, INT 1),   (5, 6, INT 0),   (5, 7, INT 0),
	      (6, 0, DIVZ),    (6, 1, INT 6),   (6, 2, INT 3),   (6, 3, INT 2),
	      (6, 4, INT 1),   (6, 5, INT 1),   (6, 6, INT 1),   (6, 7, INT 0),
	      (7, 0, DIVZ),    (7, 1, INT 7),   (7, 2, INT 3),   (7, 3, INT 2),
	      (7, 4, INT 1),   (7, 5, INT 1),   (7, 6, INT 1),   (7, 7, INT 1)
	    ];
	  List.app mod' [
	      (0, 0, DIVZ),    (0, 1, INT 0),   (0, 2, INT 0),   (0, 3, INT 0),
	      (0, 4, INT 0),   (0, 5, INT 0),   (0, 6, INT 0),   (0, 7, INT 0),
	      (1, 0, DIVZ),    (1, 1, INT 0),   (1, 2, INT 1),   (1, 3, INT 1),
	      (1, 4, INT 1),   (1, 5, INT 1),   (1, 6, INT 1),   (1, 7, INT 1),
	      (2, 0, DIVZ),    (2, 1, INT 0),   (2, 2, INT 0),   (2, 3, INT 2),
	      (2, 4, INT 2),   (2, 5, INT 2),   (2, 6, INT 2),   (2, 7, INT 2),
	      (3, 0, DIVZ),    (3, 1, INT 0),   (3, 2, INT 1),   (3, 3, INT 0),
	      (3, 4, INT 3),   (3, 5, INT 3),   (3, 6, INT 3),   (3, 7, INT 3),
	      (4, 0, DIVZ),    (4, 1, INT 0),   (4, 2, INT 0),   (4, 3, INT 1),
	      (4, 4, INT 0),   (4, 5, INT 4),   (4, 6, INT 4),   (4, 7, INT 4),
	      (5, 0, DIVZ),    (5, 1, INT 0),   (5, 2, INT 1),   (5, 3, INT 2),
	      (5, 4, INT 1),   (5, 5, INT 0),   (5, 6, INT 5),   (5, 7, INT 5),
	      (6, 0, DIVZ),    (6, 1, INT 0),   (6, 2, INT 0),   (6, 3, INT 0),
	      (6, 4, INT 2),   (6, 5, INT 1),   (6, 6, INT 0),   (6, 7, INT 6),
	      (7, 0, DIVZ),    (7, 1, INT 0),   (7, 2, INT 1),   (7, 3, INT 1),
	      (7, 4, INT 3),   (7, 5, INT 2),   (7, 6, INT 1),   (7, 7, INT 0)
	    ];
          List.app uShL [
	      (0, 0, INT 0),   (0, 1, INT 0),   (0, 2, INT 0),   (0, 3, INT 0),
	      (1, 0, INT 1),   (1, 1, INT 2),   (1, 2, INT 4),   (1, 3, OVFLW),
	      (2, 0, INT 2),   (2, 1, INT 4),   (2, 2, OVFLW),   (2, 3, OVFLW),
	      (3, 0, INT 3),   (3, 1, INT 6),   (3, 2, OVFLW),   (3, 3, OVFLW),
	      (4, 0, INT 4),   (4, 1, OVFLW),   (4, 2, OVFLW),   (4, 3, OVFLW),
	      (5, 0, INT 5),   (5, 1, OVFLW),   (5, 2, OVFLW),   (5, 3, OVFLW),
	      (6, 0, INT 6),   (6, 1, OVFLW),   (6, 2, OVFLW),   (6, 3, OVFLW),
	      (7, 0, INT 7),   (7, 1, OVFLW),   (7, 2, OVFLW),   (7, 3, OVFLW)
	    ];
          List.app uShR [
	      (0, 0, INT 0),   (0, 1, INT 0),   (0, 2, INT 0),   (0, 3, INT 0),
	      (1, 0, INT 1),   (1, 1, INT 0),   (1, 2, INT 0),   (1, 3, INT 0),
	      (2, 0, INT 2),   (2, 1, INT 1),   (2, 2, INT 0),   (2, 3, INT 0),
	      (3, 0, INT 3),   (3, 1, INT 1),   (3, 2, INT 0),   (3, 3, INT 0),
	      (4, 0, INT 4),   (4, 1, INT 2),   (4, 2, INT 1),   (4, 3, INT 0),
	      (5, 0, INT 5),   (5, 1, INT 2),   (5, 2, INT 1),   (5, 3, INT 0),
	      (6, 0, INT 6),   (6, 1, INT 3),   (6, 2, INT 1),   (6, 3, INT 0),
	      (7, 0, INT 7),   (7, 1, INT 3),   (7, 2, INT 1),   (7, 3, INT 0)
	    ];
	  List.app neg [
	      (0, INT 0), (1, INT 7), (2, INT 6), (3, INT 5), (4, INT 4),
	      (5, INT 3), (6, INT 2), (7, INT 1)
	    ];
	  List.app eq [
	      (~4, ~4, TRUE), (~4, ~3, FALS), (~4, ~2, FALS), (~4, ~1, FALS),
	      (~4,  0, FALS), (~4,  1, FALS), (~4,  2, FALS), (~4,  3, FALS),
	      (~3, ~4, FALS), (~3, ~3, TRUE), (~3, ~2, FALS), (~3, ~1, FALS),
	      (~3,  0, FALS), (~3,  1, FALS), (~3,  2, FALS), (~3,  3, FALS),
              (~4,  0, FALS), (~4,  1, FALS), (~4,  2, FALS), (~4,  3, FALS),
              (~4,  4, TRUE), (~4,  5, FALS), (~4,  6, FALS), (~4,  7, FALS),
              (~3,  0, FALS), (~3,  1, FALS), (~3,  2, FALS), (~3,  3, FALS),
              (~3,  4, FALS), (~3,  5, TRUE), (~3,  6, FALS), (~3,  7, FALS),
              (~2,  0, FALS), (~2,  1, FALS), (~2,  2, FALS), (~2,  3, FALS),
              (~2,  4, FALS), (~2,  5, FALS), (~2,  6, TRUE), (~2,  7, FALS),
              (~1,  0, FALS), (~1,  1, FALS), (~1,  2, FALS), (~1,  3, FALS),
              (~1,  4, FALS), (~1,  5, FALS), (~1,  6, FALS), (~1,  7, TRUE),
	      (0, 0, TRUE),   (0, 1, FALS),   (0, 2, FALS),   (0, 3, FALS),
	      (0, 4, FALS),   (0, 5, FALS),   (0, 6, FALS),   (0, 7, FALS),
	      (1, 0, FALS),   (1, 1, TRUE),   (1, 2, FALS),   (1, 3, FALS),
	      (1, 4, FALS),   (1, 5, FALS),   (1, 6, FALS),   (1, 7, FALS),
	      (2, 0, FALS),   (2, 1, FALS),   (2, 2, TRUE),   (2, 3, FALS),
	      (2, 4, FALS),   (2, 5, FALS),   (2, 6, FALS),   (2, 7, FALS),
	      (3, 0, FALS),   (3, 1, FALS),   (3, 2, FALS),   (3, 3, TRUE),
	      (3, 4, FALS),   (3, 5, FALS),   (3, 6, FALS),   (3, 7, FALS),
	      (4, 0, FALS),   (4, 1, FALS),   (4, 2, FALS),   (4, 3, FALS),
	      (4, 4, TRUE),   (4, 5, FALS),   (4, 6, FALS),   (4, 7, FALS),
	      (5, 0, FALS),   (5, 1, FALS),   (5, 2, FALS),   (5, 3, FALS),
	      (5, 4, FALS),   (5, 5, TRUE),   (5, 6, FALS),   (5, 7, FALS),
	      (6, 0, FALS),   (6, 1, FALS),   (6, 2, FALS),   (6, 3, FALS),
	      (6, 4, FALS),   (6, 5, FALS),   (6, 6, TRUE),   (6, 7, FALS),
	      (7, 0, FALS),   (7, 1, FALS),   (7, 2, FALS),   (7, 3, FALS),
	      (7, 4, FALS),   (7, 5, FALS),   (7, 6, FALS),   (7, 7, TRUE)
	    ];
	  List.app less [
	      (~4, ~4, FALS), (~4, ~3, TRUE), (~4, ~2, TRUE), (~4, ~1, TRUE),
	      (~4,  0, FALS), (~4,  1, FALS), (~4,  2, FALS), (~4,  3, FALS),
	      (~3, ~4, FALS), (~3, ~3, FALS), (~3, ~2, TRUE), (~3, ~1, TRUE),
	      (~3,  0, FALS), (~3,  1, FALS), (~3,  2, FALS), (~3,  3, FALS),
	      (0, 0, FALS),   (0, 1, TRUE),   (0, 2, TRUE),   (0, 3, TRUE),
	      (0, 4, TRUE),   (0, 5, TRUE),   (0, 6, TRUE),   (0, 7, TRUE),
	      (1, 0, FALS),   (1, 1, FALS),   (1, 2, TRUE),   (1, 3, TRUE),
	      (1, 4, TRUE),   (1, 5, TRUE),   (1, 6, TRUE),   (1, 7, TRUE),
	      (2, 0, FALS),   (2, 1, FALS),   (2, 2, FALS),   (2, 3, TRUE),
	      (2, 4, TRUE),   (2, 5, TRUE),   (2, 6, TRUE),   (2, 7, TRUE),
	      (3, 0, FALS),   (3, 1, FALS),   (3, 2, FALS),   (3, 3, FALS),
	      (3, 4, TRUE),   (3, 5, TRUE),   (3, 6, TRUE),   (3, 7, TRUE),
	      (4, 0, FALS),   (4, 1, FALS),   (4, 2, FALS),   (4, 3, FALS),
	      (4, 4, FALS),   (4, 5, TRUE),   (4, 6, TRUE),   (4, 7, TRUE),
	      (5, 0, FALS),   (5, 1, FALS),   (5, 2, FALS),   (5, 3, FALS),
	      (5, 4, FALS),   (5, 5, FALS),   (5, 6, TRUE),   (5, 7, TRUE),
	      (6, 0, FALS),   (6, 1, FALS),   (6, 2, FALS),   (6, 3, FALS),
	      (6, 4, FALS),   (6, 5, FALS),   (6, 6, FALS),   (6, 7, TRUE),
	      (7, 0, FALS),   (7, 1, FALS),   (7, 2, FALS),   (7, 3, FALS),
	      (7, 4, FALS),   (7, 5, FALS),   (7, 6, FALS),   (7, 7, FALS)
	    ];
	  List.app lessEq [
	      (~4, ~4, TRUE), (~4, ~3, TRUE), (~4, ~2, TRUE), (~4, ~1, TRUE),
	      (~4,  0, FALS), (~4,  1, FALS), (~4,  2, FALS), (~4,  3, FALS),
	      (~3, ~4, FALS), (~3, ~3, TRUE), (~3, ~2, TRUE), (~3, ~1, TRUE),
	      (~3,  0, FALS), (~3,  1, FALS), (~3,  2, FALS), (~3,  3, FALS),
	      (0, 0, TRUE),   (0, 1, TRUE),   (0, 2, TRUE),   (0, 3, TRUE),
	      (0, 4, TRUE),   (0, 5, TRUE),   (0, 6, TRUE),   (0, 7, TRUE),
	      (1, 0, FALS),   (1, 1, TRUE),   (1, 2, TRUE),   (1, 3, TRUE),
	      (1, 4, TRUE),   (1, 5, TRUE),   (1, 6, TRUE),   (1, 7, TRUE),
	      (2, 0, FALS),   (2, 1, FALS),   (2, 2, TRUE),   (2, 3, TRUE),
	      (2, 4, TRUE),   (2, 5, TRUE),   (2, 6, TRUE),   (2, 7, TRUE),
	      (3, 0, FALS),   (3, 1, FALS),   (3, 2, FALS),   (3, 3, TRUE),
	      (3, 4, TRUE),   (3, 5, TRUE),   (3, 6, TRUE),   (3, 7, TRUE),
	      (4, 0, FALS),   (4, 1, FALS),   (4, 2, FALS),   (4, 3, FALS),
	      (4, 4, TRUE),   (4, 5, TRUE),   (4, 6, TRUE),   (4, 7, TRUE),
	      (5, 0, FALS),   (5, 1, FALS),   (5, 2, FALS),   (5, 3, FALS),
	      (5, 4, FALS),   (5, 5, TRUE),   (5, 6, TRUE),   (5, 7, TRUE),
	      (6, 0, FALS),   (6, 1, FALS),   (6, 2, FALS),   (6, 3, FALS),
	      (6, 4, FALS),   (6, 5, FALS),   (6, 6, TRUE),   (6, 7, TRUE),
	      (7, 0, FALS),   (7, 1, FALS),   (7, 2, FALS),   (7, 3, FALS),
	      (7, 4, FALS),   (7, 5, FALS),   (7, 6, FALS),   (7, 7, TRUE)
	    ])
    end (* local *)
  end (* TestUnsignedTrapping *)

structure TestUnsignedWrapping =
  struct
    local
      datatype z = datatype TestUtil.value
      structure A = UnsignedWrappingArith
      val check1 = TestUtil.check1 "UnsignedWrappingArith" 3
      val check2 = TestUtil.check2 "UnsignedWrappingArith" 3
      val checkCmp = TestUtil.checkCmp "UnsignedWrappingArith" 3
      val narrow = check1 "uNarrow" A.uNarrow
      val toUnsigned = check1 "toUnsigned" A.toUnsigned
      val add = check2 "+" A.uAdd
      val sub = check2 "-" A.uSub
      val mul = check2 "*" A.uMul
      val div' = check2 "div" A.uDiv
      val mod' = check2 "mod" A.uMod
      val uShL = check2 "uShL" A.uShL
      val uShR = check2 "uShR" A.uShR
      val neg = check1 "~" A.uNeg
      val eq = checkCmp "=" A.uEq
      val less = checkCmp "<" A.uLess
      val lessEq = checkCmp "<=" A.uLessEq
      val DIVZ = DIV_EXN
      val TRUE = BOOL true
      val FALS = BOOL false
    in
    fun test () = (
	  List.app narrow [
	      (0, INT 0), (1, INT 1), (2, INT 2), (3, INT 3), (4, INT 4),
	      (5, INT 5), (6, INT 6), (7, INT 7), (8, INT 0), (9, INT 1)
	    ];
	  List.app toUnsigned [
	      (~5, INT 3), (~4, INT 4), (~3, INT 5), (~2, INT 6), (~1, INT 7),
	      ( 0, INT 0), ( 1, INT 1), ( 2, INT 2), ( 3, INT 3), ( 4, INT 4)
	    ];
	  List.app add [
	      (0, 0, INT 0),   (0, 1, INT 1),   (0, 2, INT 2),   (0, 3, INT 3),
	      (0, 4, INT 4),   (0, 5, INT 5),   (0, 6, INT 6),   (0, 7, INT 7),
	      (1, 0, INT 1),   (1, 1, INT 2),   (1, 2, INT 3),   (1, 3, INT 4),
	      (1, 4, INT 5),   (1, 5, INT 6),   (1, 6, INT 7),   (1, 7, INT 0),
	      (2, 0, INT 2),   (2, 1, INT 3),   (2, 2, INT 4),   (2, 3, INT 5),
	      (2, 4, INT 6),   (2, 5, INT 7),   (2, 6, INT 0),   (2, 7, INT 1),
	      (3, 0, INT 3),   (3, 1, INT 4),   (3, 2, INT 5),   (3, 3, INT 6),
	      (3, 4, INT 7),   (3, 5, INT 0),   (3, 6, INT 1),   (3, 7, INT 2),
	      (4, 0, INT 4),   (4, 1, INT 5),   (4, 2, INT 6),   (4, 3, INT 7),
	      (4, 4, INT 0),   (4, 5, INT 1),   (4, 6, INT 2),   (4, 7, INT 3),
	      (5, 0, INT 5),   (5, 1, INT 6),   (5, 2, INT 7),   (5, 3, INT 0),
	      (5, 4, INT 1),   (5, 5, INT 2),   (5, 6, INT 3),   (5, 7, INT 4),
	      (6, 0, INT 6),   (6, 1, INT 7),   (6, 2, INT 0),   (6, 3, INT 1),
	      (6, 4, INT 2),   (6, 5, INT 3),   (6, 6, INT 4),   (6, 7, INT 5),
	      (7, 0, INT 7),   (7, 1, INT 0),   (7, 2, INT 1),   (7, 3, INT 2),
	      (7, 4, INT 3),   (7, 5, INT 4),   (7, 6, INT 5),   (7, 7, INT 6)
	    ];
	  List.app sub [
	      (0, 0, INT 0),   (0, 1, INT 7),   (0, 2, INT 6),   (0, 3, INT 5),
	      (0, 4, INT 4),   (0, 5, INT 3),   (0, 6, INT 2),   (0, 7, INT 1),
	      (1, 0, INT 1),   (1, 1, INT 0),   (1, 2, INT 7),   (1, 3, INT 6),
	      (1, 4, INT 5),   (1, 5, INT 4),   (1, 6, INT 3),   (1, 7, INT 2),
	      (2, 0, INT 2),   (2, 1, INT 1),   (2, 2, INT 0),   (2, 3, INT 7),
	      (2, 4, INT 6),   (2, 5, INT 5),   (2, 6, INT 4),   (2, 7, INT 3),
	      (3, 0, INT 3),   (3, 1, INT 2),   (3, 2, INT 1),   (3, 3, INT 0),
	      (3, 4, INT 7),   (3, 5, INT 6),   (3, 6, INT 5),   (3, 7, INT 4),
	      (4, 0, INT 4),   (4, 1, INT 3),   (4, 2, INT 2),   (4, 3, INT 1),
	      (4, 4, INT 0),   (4, 5, INT 7),   (4, 6, INT 6),   (4, 7, INT 5),
	      (5, 0, INT 5),   (5, 1, INT 4),   (5, 2, INT 3),   (5, 3, INT 2),
	      (5, 4, INT 1),   (5, 5, INT 0),   (5, 6, INT 7),   (5, 7, INT 6),
	      (6, 0, INT 6),   (6, 1, INT 5),   (6, 2, INT 4),   (6, 3, INT 3),
	      (6, 4, INT 2),   (6, 5, INT 1),   (6, 6, INT 0),   (6, 7, INT 7),
	      (7, 0, INT 7),   (7, 1, INT 6),   (7, 2, INT 5),   (7, 3, INT 4),
	      (7, 4, INT 3),   (7, 5, INT 2),   (7, 6, INT 1),   (7, 7, INT 0)
	    ];
	  List.app mul [
	      (0, 0, INT 0),   (0, 1, INT 0),   (0, 2, INT 0),   (0, 3, INT 0),
	      (0, 4, INT 0),   (0, 5, INT 0),   (0, 6, INT 0),   (0, 7, INT 0),
	      (1, 0, INT 0),   (1, 1, INT 1),   (1, 2, INT 2),   (1, 3, INT 3),
	      (1, 4, INT 4),   (1, 5, INT 5),   (1, 6, INT 6),   (1, 7, INT 7),
	      (2, 0, INT 0),   (2, 1, INT 2),   (2, 2, INT 4),   (2, 3, INT 6),
	      (2, 4, INT 0),   (2, 5, INT 2),   (2, 6, INT 4),   (2, 7, INT 6),
	      (3, 0, INT 0),   (3, 1, INT 3),   (3, 2, INT 6),   (3, 3, INT 1),
	      (3, 4, INT 4),   (3, 5, INT 7),   (3, 6, INT 2),   (3, 7, INT 5),
	      (4, 0, INT 0),   (4, 1, INT 4),   (4, 2, INT 0),   (4, 3, INT 4),
	      (4, 4, INT 0),   (4, 5, INT 4),   (4, 6, INT 0),   (4, 7, INT 4),
	      (5, 0, INT 0),   (5, 1, INT 5),   (5, 2, INT 2),   (5, 3, INT 7),
	      (5, 4, INT 4),   (5, 5, INT 1),   (5, 6, INT 6),   (5, 7, INT 3),
	      (6, 0, INT 0),   (6, 1, INT 6),   (6, 2, INT 4),   (6, 3, INT 2),
	      (6, 4, INT 0),   (6, 5, INT 6),   (6, 6, INT 4),   (6, 7, INT 2),
	      (7, 0, INT 0),   (7, 1, INT 7),   (7, 2, INT 6),   (7, 3, INT 5),
	      (7, 4, INT 4),   (7, 5, INT 3),   (7, 6, INT 2),   (7, 7, INT 1)
	    ];
	  List.app div' [
	      (0, 0, DIVZ),    (0, 1, INT 0),   (0, 2, INT 0),   (0, 3, INT 0),
	      (0, 4, INT 0),   (0, 5, INT 0),   (0, 6, INT 0),   (0, 7, INT 0),
	      (1, 0, DIVZ),    (1, 1, INT 1),   (1, 2, INT 0),   (1, 3, INT 0),
	      (1, 4, INT 0),   (1, 5, INT 0),   (1, 6, INT 0),   (1, 7, INT 0),
	      (2, 0, DIVZ),    (2, 1, INT 2),   (2, 2, INT 1),   (2, 3, INT 0),
	      (2, 4, INT 0),   (2, 5, INT 0),   (2, 6, INT 0),   (2, 7, INT 0),
	      (3, 0, DIVZ),    (3, 1, INT 3),   (3, 2, INT 1),   (3, 3, INT 1),
	      (3, 4, INT 0),   (3, 5, INT 0),   (3, 6, INT 0),   (3, 7, INT 0),
	      (4, 0, DIVZ),    (4, 1, INT 4),   (4, 2, INT 2),   (4, 3, INT 1),
	      (4, 4, INT 1),   (4, 5, INT 0),   (4, 6, INT 0),   (4, 7, INT 0),
	      (5, 0, DIVZ),    (5, 1, INT 5),   (5, 2, INT 2),   (5, 3, INT 1),
	      (5, 4, INT 1),   (5, 5, INT 1),   (5, 6, INT 0),   (5, 7, INT 0),
	      (6, 0, DIVZ),    (6, 1, INT 6),   (6, 2, INT 3),   (6, 3, INT 2),
	      (6, 4, INT 1),   (6, 5, INT 1),   (6, 6, INT 1),   (6, 7, INT 0),
	      (7, 0, DIVZ),    (7, 1, INT 7),   (7, 2, INT 3),   (7, 3, INT 2),
	      (7, 4, INT 1),   (7, 5, INT 1),   (7, 6, INT 1),   (7, 7, INT 1)
	    ];
	  List.app mod' [
	      (0, 0, DIVZ),    (0, 1, INT 0),   (0, 2, INT 0),   (0, 3, INT 0),
	      (0, 4, INT 0),   (0, 5, INT 0),   (0, 6, INT 0),   (0, 7, INT 0),
	      (1, 0, DIVZ),    (1, 1, INT 0),   (1, 2, INT 1),   (1, 3, INT 1),
	      (1, 4, INT 1),   (1, 5, INT 1),   (1, 6, INT 1),   (1, 7, INT 1),
	      (2, 0, DIVZ),    (2, 1, INT 0),   (2, 2, INT 0),   (2, 3, INT 2),
	      (2, 4, INT 2),   (2, 5, INT 2),   (2, 6, INT 2),   (2, 7, INT 2),
	      (3, 0, DIVZ),    (3, 1, INT 0),   (3, 2, INT 1),   (3, 3, INT 0),
	      (3, 4, INT 3),   (3, 5, INT 3),   (3, 6, INT 3),   (3, 7, INT 3),
	      (4, 0, DIVZ),    (4, 1, INT 0),   (4, 2, INT 0),   (4, 3, INT 1),
	      (4, 4, INT 0),   (4, 5, INT 4),   (4, 6, INT 4),   (4, 7, INT 4),
	      (5, 0, DIVZ),    (5, 1, INT 0),   (5, 2, INT 1),   (5, 3, INT 2),
	      (5, 4, INT 1),   (5, 5, INT 0),   (5, 6, INT 5),   (5, 7, INT 5),
	      (6, 0, DIVZ),    (6, 1, INT 0),   (6, 2, INT 0),   (6, 3, INT 0),
	      (6, 4, INT 2),   (6, 5, INT 1),   (6, 6, INT 0),   (6, 7, INT 6),
	      (7, 0, DIVZ),    (7, 1, INT 0),   (7, 2, INT 1),   (7, 3, INT 1),
	      (7, 4, INT 3),   (7, 5, INT 2),   (7, 6, INT 1),   (7, 7, INT 0)
	    ];
          List.app uShL [
	      (0, 0, INT 0),   (0, 1, INT 0),   (0, 2, INT 0),   (0, 3, INT 0),
	      (1, 0, INT 1),   (1, 1, INT 2),   (1, 2, INT 4),   (1, 3, INT 0),
	      (2, 0, INT 2),   (2, 1, INT 4),   (2, 2, INT 0),   (2, 3, INT 0),
	      (3, 0, INT 3),   (3, 1, INT 6),   (3, 2, INT 4),   (3, 3, INT 0),
	      (4, 0, INT 4),   (4, 1, INT 0),   (4, 2, INT 0),   (4, 3, INT 0),
	      (5, 0, INT 5),   (5, 1, INT 2),   (5, 2, INT 4),   (5, 3, INT 0),
	      (6, 0, INT 6),   (6, 1, INT 4),   (6, 2, INT 0),   (6, 3, INT 0),
	      (7, 0, INT 7),   (7, 1, INT 6),   (7, 2, INT 4),   (7, 3, INT 0)
	    ];
          List.app uShR [
	      (0, 0, INT 0),   (0, 1, INT 0),   (0, 2, INT 0),   (0, 3, INT 0),
	      (1, 0, INT 1),   (1, 1, INT 0),   (1, 2, INT 0),   (1, 3, INT 0),
	      (2, 0, INT 2),   (2, 1, INT 1),   (2, 2, INT 0),   (2, 3, INT 0),
	      (3, 0, INT 3),   (3, 1, INT 1),   (3, 2, INT 0),   (3, 3, INT 0),
	      (4, 0, INT 4),   (4, 1, INT 2),   (4, 2, INT 1),   (4, 3, INT 0),
	      (5, 0, INT 5),   (5, 1, INT 2),   (5, 2, INT 1),   (5, 3, INT 0),
	      (6, 0, INT 6),   (6, 1, INT 3),   (6, 2, INT 1),   (6, 3, INT 0),
	      (7, 0, INT 7),   (7, 1, INT 3),   (7, 2, INT 1),   (7, 3, INT 0)
	    ];
	  List.app neg [
	      (0, INT 0), (1, INT 7), (2, INT 6), (3, INT 5), (4, INT 4),
	      (5, INT 3), (6, INT 2), (7, INT 1)
	    ];
	  List.app eq [
	      (~4, ~4, TRUE), (~4, ~3, FALS), (~4, ~2, FALS), (~4, ~1, FALS),
	      (~4,  0, FALS), (~4,  1, FALS), (~4,  2, FALS), (~4,  3, FALS),
	      (~3, ~4, FALS), (~3, ~3, TRUE), (~3, ~2, FALS), (~3, ~1, FALS),
	      (~3,  0, FALS), (~3,  1, FALS), (~3,  2, FALS), (~3,  3, FALS),
              (~4,  0, FALS), (~4,  1, FALS), (~4,  2, FALS), (~4,  3, FALS),
              (~4,  4, TRUE), (~4,  5, FALS), (~4,  6, FALS), (~4,  7, FALS),
              (~3,  0, FALS), (~3,  1, FALS), (~3,  2, FALS), (~3,  3, FALS),
              (~3,  4, FALS), (~3,  5, TRUE), (~3,  6, FALS), (~3,  7, FALS),
              (~2,  0, FALS), (~2,  1, FALS), (~2,  2, FALS), (~2,  3, FALS),
              (~2,  4, FALS), (~2,  5, FALS), (~2,  6, TRUE), (~2,  7, FALS),
              (~1,  0, FALS), (~1,  1, FALS), (~1,  2, FALS), (~1,  3, FALS),
              (~1,  4, FALS), (~1,  5, FALS), (~1,  6, FALS), (~1,  7, TRUE),
	      (0, 0, TRUE),   (0, 1, FALS),   (0, 2, FALS),   (0, 3, FALS),
	      (0, 4, FALS),   (0, 5, FALS),   (0, 6, FALS),   (0, 7, FALS),
	      (1, 0, FALS),   (1, 1, TRUE),   (1, 2, FALS),   (1, 3, FALS),
	      (1, 4, FALS),   (1, 5, FALS),   (1, 6, FALS),   (1, 7, FALS),
	      (2, 0, FALS),   (2, 1, FALS),   (2, 2, TRUE),   (2, 3, FALS),
	      (2, 4, FALS),   (2, 5, FALS),   (2, 6, FALS),   (2, 7, FALS),
	      (3, 0, FALS),   (3, 1, FALS),   (3, 2, FALS),   (3, 3, TRUE),
	      (3, 4, FALS),   (3, 5, FALS),   (3, 6, FALS),   (3, 7, FALS),
	      (4, 0, FALS),   (4, 1, FALS),   (4, 2, FALS),   (4, 3, FALS),
	      (4, 4, TRUE),   (4, 5, FALS),   (4, 6, FALS),   (4, 7, FALS),
	      (5, 0, FALS),   (5, 1, FALS),   (5, 2, FALS),   (5, 3, FALS),
	      (5, 4, FALS),   (5, 5, TRUE),   (5, 6, FALS),   (5, 7, FALS),
	      (6, 0, FALS),   (6, 1, FALS),   (6, 2, FALS),   (6, 3, FALS),
	      (6, 4, FALS),   (6, 5, FALS),   (6, 6, TRUE),   (6, 7, FALS),
	      (7, 0, FALS),   (7, 1, FALS),   (7, 2, FALS),   (7, 3, FALS),
	      (7, 4, FALS),   (7, 5, FALS),   (7, 6, FALS),   (7, 7, TRUE)
	    ];
	  List.app less [
	      (~4, ~4, FALS), (~4, ~3, TRUE), (~4, ~2, TRUE), (~4, ~1, TRUE),
	      (~4,  0, FALS), (~4,  1, FALS), (~4,  2, FALS), (~4,  3, FALS),
	      (~3, ~4, FALS), (~3, ~3, FALS), (~3, ~2, TRUE), (~3, ~1, TRUE),
	      (~3,  0, FALS), (~3,  1, FALS), (~3,  2, FALS), (~3,  3, FALS),
	      (0, 0, FALS),   (0, 1, TRUE),   (0, 2, TRUE),   (0, 3, TRUE),
	      (0, 4, TRUE),   (0, 5, TRUE),   (0, 6, TRUE),   (0, 7, TRUE),
	      (1, 0, FALS),   (1, 1, FALS),   (1, 2, TRUE),   (1, 3, TRUE),
	      (1, 4, TRUE),   (1, 5, TRUE),   (1, 6, TRUE),   (1, 7, TRUE),
	      (2, 0, FALS),   (2, 1, FALS),   (2, 2, FALS),   (2, 3, TRUE),
	      (2, 4, TRUE),   (2, 5, TRUE),   (2, 6, TRUE),   (2, 7, TRUE),
	      (3, 0, FALS),   (3, 1, FALS),   (3, 2, FALS),   (3, 3, FALS),
	      (3, 4, TRUE),   (3, 5, TRUE),   (3, 6, TRUE),   (3, 7, TRUE),
	      (4, 0, FALS),   (4, 1, FALS),   (4, 2, FALS),   (4, 3, FALS),
	      (4, 4, FALS),   (4, 5, TRUE),   (4, 6, TRUE),   (4, 7, TRUE),
	      (5, 0, FALS),   (5, 1, FALS),   (5, 2, FALS),   (5, 3, FALS),
	      (5, 4, FALS),   (5, 5, FALS),   (5, 6, TRUE),   (5, 7, TRUE),
	      (6, 0, FALS),   (6, 1, FALS),   (6, 2, FALS),   (6, 3, FALS),
	      (6, 4, FALS),   (6, 5, FALS),   (6, 6, FALS),   (6, 7, TRUE),
	      (7, 0, FALS),   (7, 1, FALS),   (7, 2, FALS),   (7, 3, FALS),
	      (7, 4, FALS),   (7, 5, FALS),   (7, 6, FALS),   (7, 7, FALS)
	    ];
	  List.app lessEq [
	      (~4, ~4, TRUE), (~4, ~3, TRUE), (~4, ~2, TRUE), (~4, ~1, TRUE),
	      (~4,  0, FALS), (~4,  1, FALS), (~4,  2, FALS), (~4,  3, FALS),
	      (~3, ~4, FALS), (~3, ~3, TRUE), (~3, ~2, TRUE), (~3, ~1, TRUE),
	      (~3,  0, FALS), (~3,  1, FALS), (~3,  2, FALS), (~3,  3, FALS),
	      (0, 0, TRUE),   (0, 1, TRUE),   (0, 2, TRUE),   (0, 3, TRUE),
	      (0, 4, TRUE),   (0, 5, TRUE),   (0, 6, TRUE),   (0, 7, TRUE),
	      (1, 0, FALS),   (1, 1, TRUE),   (1, 2, TRUE),   (1, 3, TRUE),
	      (1, 4, TRUE),   (1, 5, TRUE),   (1, 6, TRUE),   (1, 7, TRUE),
	      (2, 0, FALS),   (2, 1, FALS),   (2, 2, TRUE),   (2, 3, TRUE),
	      (2, 4, TRUE),   (2, 5, TRUE),   (2, 6, TRUE),   (2, 7, TRUE),
	      (3, 0, FALS),   (3, 1, FALS),   (3, 2, FALS),   (3, 3, TRUE),
	      (3, 4, TRUE),   (3, 5, TRUE),   (3, 6, TRUE),   (3, 7, TRUE),
	      (4, 0, FALS),   (4, 1, FALS),   (4, 2, FALS),   (4, 3, FALS),
	      (4, 4, TRUE),   (4, 5, TRUE),   (4, 6, TRUE),   (4, 7, TRUE),
	      (5, 0, FALS),   (5, 1, FALS),   (5, 2, FALS),   (5, 3, FALS),
	      (5, 4, FALS),   (5, 5, TRUE),   (5, 6, TRUE),   (5, 7, TRUE),
	      (6, 0, FALS),   (6, 1, FALS),   (6, 2, FALS),   (6, 3, FALS),
	      (6, 4, FALS),   (6, 5, FALS),   (6, 6, TRUE),   (6, 7, TRUE),
	      (7, 0, FALS),   (7, 1, FALS),   (7, 2, FALS),   (7, 3, FALS),
	      (7, 4, FALS),   (7, 5, FALS),   (7, 6, FALS),   (7, 7, TRUE)
	    ])
    end (* local *)
  end (* TestUnsignedWrapping *)

structure TestAll =
  struct

    fun test () = (
	  TestBitwiseArith.test();
	  TestSignedTrapping.test();
	  TestSignedWrapping.test();
	  TestUnsignedTrapping.test();
	  TestUnsignedWrapping.test())

(*
    val _ = test()
*)

  end

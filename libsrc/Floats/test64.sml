(* test64.sml
 *
 * Test cases for converting float literals to IEEE double-precision floats (64-bit)
 * See http://www.binaryconvert.com/result_double.html for an online converter.
 *
 * COPYRIGHT (c) 2016 John Reppy (http://cs.uchicago.edu/~jhr)
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

CM.make "sources.cm";

structure F64ToBits = FloatToBitsFn (IEEEFloat64Params);

fun equalVec (v1, v2) = (case Word8Vector.collate Word8.compare (v1, v2)
       of EQUAL => true
	| _ => false
      (* end case *))

fun bits2s vec = String.concatWithMap
      "_" (fn w => StringCvt.padLeft #"0" 8 (Word8.fmt StringCvt.BIN w))
      (Word8Vector.toList vec)

fun check (lit, expected) = let
      val (res, _) = F64ToBits.toBits lit
      in
	print (concat (
	  "check " :: FloatLit.toString lit ::
	  (if equalVec (res, expected)
	    then [" ok\n"]
	    else [
		" failed\n",
		"  expected: ", bits2s expected, "\n",
		"  result:   ", bits2s res, "\n"
	      ])))
      end
	handle _ => print "failed (exception)\n"

fun check' (sign, digits, exp, expected) =
      check (
	FloatLit.fromDigits {isNeg = sign, digits = digits, exp = exp},
	Word8Vector.fromList expected);

val _ = (
      (* 0 *)
	check' (false, [0], 0,  [0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00]);
      (* -0 *)
	check' (true, [0], 0,   [0wx80, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00]);
      (* 1 *)
	check' (false, [1], 1,  [0wx3f, 0wxf0, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00]);
      (* 2 *)
	check' (false, [2], 1,  [0wx40, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00]);
      (* -2 *)
	check' (true, [2], 1,   [0wxc0, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00]);
      (* 0.25 *)
	check' (false, [2,5], 0,[0wx3f, 0wxd0, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00]);
      (* 1.5 *)
	check' (false, [1,5], 1,[0wx3F, 0wxF8, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00]);
      (* pi *)
	check (FloatLit.M_PI, Word8Vector.fromList
				[0wx40, 0wx09, 0wx21, 0wxFB, 0wx54, 0wx44, 0wx2D, 0wx18]);
      (* Min normal positive double: 2.2250738585072014 × 10^{−308} *)
	check' (false, [2,2,2,5,0,7,3,8,5,8,5,0,7,2,0,1,4], ~307,
				[0wx00, 0wx10, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00]);
      (* Max double: 1.7976931348623157 × 10^{308} *)
	check' (false, [1,7,9,7,6,9,3,1,3,4,8,6,2,3,1,5,7], 309,
				[0wx7f, 0wxef, 0wxff, 0wxff, 0wxff, 0wxff, 0wxff, 0wxff]);
	());

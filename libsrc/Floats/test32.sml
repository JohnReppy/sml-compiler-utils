(* test32.sml
 *
 * Test cases for converting float literals to IEEE double-precision floats (64-bit)
 * See http://www.binaryconvert.com/result_float.html for an online converter.
 *
 * COPYRIGHT (c) 2018 John Reppy (https://cs.uchicago.edu/~jhr)
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

CM.make "sources.cm";

structure F32ToBits = FloatToBitsFn (IEEEFloat32Params);

fun equalVec (v1, v2) = (case Word8Vector.collate Word8.compare (v1, v2)
       of EQUAL => true
        | _ => false
      (* end case *))

fun bits2s vec = String.concatWithMap
      "_" (fn w => StringCvt.padLeft #"0" 8 (Word8.fmt StringCvt.BIN w))
      (Word8Vector.toList vec)

fun check (lit, expected) = let
      val (res, _) = F32ToBits.toBits lit
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
        handle Overflow => print "failed (too large)\n"
             | ex => print(concat[" failed (", exnMessage ex, ")\n"])

fun check' (sign, digits, exp, expected) =
      check (
        FloatLit.fromDigits {isNeg = sign, digits = digits, exp = exp},
        Word8Vector.fromList expected);

val _ = (
      (* 0 *)
        check' (false, [0], 0,          [0wx00, 0wx00, 0wx00, 0wx00]);
      (* -0 *)
        check' (true, [0], 0,           [0wx80, 0wx00, 0wx00, 0wx00]);
      (* 1 *)
        check' (false, [1], 1,          [0wx3f, 0wx80, 0wx00, 0wx00]);
      (* 2 *)
        check' (false, [2], 1,          [0wx40, 0wx00, 0wx00, 0wx00]);
      (* -2 *)
        check' (true, [2], 1,           [0wxc0, 0wx00, 0wx00, 0wx00]);
      (* 0.25 *)
        check' (false, [2,5], 0,        [0wx3e, 0wx80, 0wx00, 0wx00]);
      (* 0.15 *)
        check' (false, [1,5], 0,        [0wx3e, 0wx19, 0wx99, 0wx9A]);
      (* 1.5 *)
        check' (false, [1,5], 1,        [0wx3f, 0wxc0, 0wx00, 0wx00]);
      (* pi *)
        check (FloatConstants.M_PI, Word8Vector.fromList[0wx40, 0wx49, 0wx0F, 0wxDB]);
      (* Min normal positive float: 1.175494351 × 10^{−38} *)
        check' (false, [1,1,7,5,4,9,4,3,5,1], ~37,
                                        [0wx00, 0wx80, 0wx00, 0wx00]);
      (* Max float: 3.402823466 × 10^{38} *)
        check' (false, [3,4,0,2,8,2,3,4,6,6], 39,
                                        [0wx7f, 0wx7f, 0wxff, 0wxff]);
        ());

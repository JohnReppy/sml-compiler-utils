(* test.sml
 *
 * Test cases for converting generic float literals.
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

structure F = FloatLit;

fun equalVec (v1, v2) = (case Word8Vector.collate Word8.compare (v1, v2)
       of EQUAL => true
        | _ => false
      (* end case *))

fun bits2s vec = String.concatWithMap
      "_" (fn w => StringCvt.padLeft #"0" 8 (Word8.fmt StringCvt.BIN w))
      (Word8Vector.toList vec)

(* check that serialization is working *)
local
(* check encoding *)
  fun checkEnc (lit, expected) = let
        val expected = Word8Vector.fromList expected
        val res = F.toBytes lit
        in
          print (concat (
            "check encoding " :: F.toString lit ::
            (if equalVec (res, expected)
                then [" ok\n"]
                else [
                    " failed\n",
                    "  expected: ", bits2s expected, "\n",
                    "  result:   ", bits2s res, "\n"
                  ])))
        end
          handle ex => print(concat[
              "check encoding ", F.toString lit, " failed (", exnMessage ex, ")\n"
            ])
(* check decoding *)
  fun checkDec (expected, bytes) = let
        val bytes = Word8Vector.fromList bytes
        val res = F.fromBytes bytes
        in
          print (concat (
            "check decoding " :: F.toString expected ::
            (if F.same (res, expected)
                then [" ok\n"]
                else [" failed; result = ", F.toString res, "\n"])))
        end
          handle ex => print(concat[
              "check decoding ", F.toString expected, " failed (", exnMessage ex, ")\n"
            ])
(* check round-trip *)
  fun check lit = let
        val enc = F.toBytes lit
        val dec = F.fromBytes enc
        in
          print (concat ("check encode/decode " :: F.toString lit ::
            (if F.same (lit, dec)
                then [" ok\n"]
                else [
                    " failed\n",
                    "  encoded: ", bits2s enc, "\n",
                    "  decoded: ", F.toString dec, "\n"
                  ])))
        end
          handle ex => print(concat[
              "check  encode/decode ", F.toString lit, " failed (", exnMessage ex, ")\n"
            ])
in
val _ = (
      (* 0 *)
        checkEnc (F.zero false,         [0w0, 0w0, 0w0, 0w0, 0w0]);
      (* -0 *)
        checkEnc (F.zero true,          [0w1, 0w0, 0w0, 0w0, 0w0]);
      (* 1 *)
        checkEnc (F.one,                [0w0, 0w1, 0w0, 0w0, 0w0, 0w1]);
      (* 2 *)
        checkEnc (F.fromDigits{isNeg=false, digits=[2], exp=1},
                                        [0w0, 0w2, 0w0, 0w0, 0w0, 0w1]);
      (* -2 *)
        checkEnc (F.fromDigits{isNeg=true, digits=[2], exp=1},
                                        [0w1, 0w2, 0w0, 0w0, 0w0, 0w1]);
      (* 0.25 *)
        checkEnc (F.fromDigits{isNeg=false, digits=[2,5], exp=0},
                                        [0w0, 0w2, 0w5, 0w0, 0w0, 0w0, 0w0]))
val _ = (
      (* 0 *)
        checkDec (F.zero false,         [0w0, 0w0, 0w0, 0w0, 0w0]);
      (* -0 *)
        checkDec (F.zero true,          [0w1, 0w0, 0w0, 0w0, 0w0]);
      (* 1 *)
        checkDec (F.one,                [0w0, 0w1, 0w0, 0w0, 0w0, 0w1]);
      (* 2 *)
        checkDec (F.fromDigits{isNeg=false, digits=[2], exp=1},
                                        [0w0, 0w2, 0w0, 0w0, 0w0, 0w1]);
      (* -2 *)
        checkDec (F.fromDigits{isNeg=true, digits=[2], exp=1},
                                        [0w1, 0w2, 0w0, 0w0, 0w0, 0w1]);
      (* 0.25 *)
        checkDec (F.fromDigits{isNeg=false, digits=[2,5], exp=0},
                                        [0w0, 0w2, 0w5, 0w0, 0w0, 0w0, 0w0]))
val _ = (
        check FloatConstants.M_PI)
end; (* local *)

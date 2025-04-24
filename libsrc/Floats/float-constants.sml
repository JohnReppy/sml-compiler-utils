(* float-constants.sml
 *
 * Floating-point literals for the special math constants from math.h.
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

structure FloatConstants : sig

  (* special math constants (the "M_" constants from math.h) *)
    val M_E : FloatLit.t        (* e *)
    val M_LOG2E : FloatLit.t    (* log2(e) *)
    val M_LOG10E : FloatLit.t   (* log10(e) *)
    val M_LN2 : FloatLit.t      (* ln(2) *)
    val M_LN10 : FloatLit.t     (* ln(10) *)
    val M_PI : FloatLit.t       (* pi *)
    val M_PI_2 : FloatLit.t     (* pi / 2 *)
    val M_PI_4 : FloatLit.t     (* pi / 4 *)
    val M_1_PI : FloatLit.t     (* 1 / pi *)
    val M_2_PI : FloatLit.t     (* 2 / pi *)
    val M_2_SQRTPI : FloatLit.t (* 2 / sqrt(pi) *)
    val M_SQRT2 : FloatLit.t    (* sqrt(2) *)
    val M_SQRT1_2 : FloatLit.t  (* 1 / sqrt(2) *)

  end = struct

    val M_E = FloatLit.fromDigits{
            isNeg = false,
            digits = [2,7,1,8,2,8,1,8,2,8,4,5,9,0,4,5,2,3,5,3,6,0,2,8,7,4,7,1,3,5,2,6,6,2,5,0],
            exp = 1
          }
    val M_LOG2E = FloatLit.fromDigits{
            isNeg = false,
            digits = [1,4,4,2,6,9,5,0,4,0,8,8,8,9,6,3,4,0,7,3,5,9,9,2,4,6,8,1,0,0,1,8,9,2,1,4],
            exp = 1
          }
    val M_LOG10E = FloatLit.fromDigits{
            isNeg = false,
            digits = [0,4,3,4,2,9,4,4,8,1,9,0,3,2,5,1,8,2,7,6,5,1,1,2,8,9,1,8,9,1,6,6,0,5,0,8,2],
            exp = 1
          }
    val M_LN2 = FloatLit.fromDigits{
            isNeg = false,
            digits = [6,9,3,1,4,7,1,8,0,5,5,9,9,4,5,3,0,9,4,1,7,2,3,2,1,2,1,4,5,8,1,7,6,5,6,8],
            exp = 0
          }
    val M_LN10 = FloatLit.fromDigits{
            isNeg = false,
            digits = [2,3,0,2,5,8,5,0,9,2,9,9,4,0,4,5,6,8,4,0,1,7,9,9,1,4,5,4,6,8,4,3,6,4,2,1],
            exp = 1
          }
    val M_PI = FloatLit.fromDigits{
            isNeg = false,
            digits = [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9,3,2,3,8,4,6,2,6,4,3,3,8,3,2,7,9,5,0,2,8,8],
            exp = 1
          }
    val M_PI_2 = FloatLit.fromDigits{
            isNeg = false,
            digits = [1,5,7,0,7,9,6,3,2,6,7,9,4,8,9,6,6,1,9,2,3,1,3,2,1,6,9,1,6,3,9,7,5,1,4,4],
            exp = 1
          }
    val M_PI_4 = FloatLit.fromDigits{
            isNeg = false,
            digits = [7,8,5,3,9,8,1,6,3,3,9,7,4,4,8,3,0,9,6,1,5,6,6,0,8,4,5,8,1,9,8,7,5,7,2,1],
            exp = 0
          }
    val M_1_PI = FloatLit.fromDigits{
            isNeg = false,
            digits = [3,1,8,3,0,9,8,8,6,1,8,3,7,9,0,6,7,1,5,3,7,7,6,7,5,2,6,7,4,5,0,2,8,7,2,4],
            exp = 0
          }
    val M_2_PI = FloatLit.fromDigits{
            isNeg = false,
            digits = [6,3,6,6,1,9,7,7,2,3,6,7,5,8,1,3,4,3,0,7,5,5,3,5,0,5,3,4,9,0,0,5,7,4,4,8],
            exp = 0
          }
    val M_2_SQRTPI = FloatLit.fromDigits{
            isNeg = false,
            digits = [1,1,2,8,3,7,9,1,6,7,0,9,5,5,1,2,5,7,3,8,9,6,1,5,8,9,0,3,1,2,1,5,4,5,1,7],
            exp = 1
          }
    val M_SQRT2 = FloatLit.fromDigits{
            isNeg = false,
            digits = [1,4,1,4,2,1,3,5,6,2,3,7,3,0,9,5,0,4,8,8,0,1,6,8,8,7,2,4,2,0,9,6,9,8,0,8],
            exp = 1
          }
    val M_SQRT1_2 = FloatLit.fromDigits{
            isNeg = false,
            digits = [7,0,7,1,0,6,7,8,1,1,8,6,5,4,7,5,2,4,4,0,0,8,4,4,3,6,2,1,0,4,8,4,9,0,3,9],
            exp = 0
          }

  end

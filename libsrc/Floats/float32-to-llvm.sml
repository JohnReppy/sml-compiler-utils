(* float32-to-llvm-fn.sml
 *
 * This module supports converting 32-bit float literals to 8-byte bit
 * strings suitable for emitting into an LLVM assembler file.
 *
 * The LLVM assembler expects floating-point literals to be either decimal
 * literals that are exactly representable in floating-point, or hexidecimal
 * bit strings.  For 16, 32, and 64-bit floats, the bit strings must be
 * written as 16-digit hexidecimal numbers.  This requirement means that
 * the bit representation of the exponent and mantissa must be converted
 * to double-precision format.
 *
 * This code is based on code contributed by Kavon Farvardin.
 *
 * COPYRIGHT (c) 2018 John Reppy (http://cs.uchicago.edu/~jhr)
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

structure Float32ToLLVM : FLOAT_TO_BITS =
  struct

    structure W8A = Word8Array
    structure W32 = Word32
    structure P32 = PackWord32Big

    structure F32ToBits = FloatToBitsFn (IEEEFloat32Params)

    val ++ = W32.orb
    val & = W32.andb
    infix 6 ++
    infix 7 &

  (* versions of the PackWord operations that are guaranteed to have Word32.word
   * as their argument type (instead of LargeWord.word)
   *)
    val subVec = W32.fromLarge o P32.subVec
    fun update (arr, i, v) = P32.update(arr, i, W32.toLarge v)

  (* offset for converting from the 16-bit exponent-bias (15) to 64-bit bias *)
    val biasOffset = W32.fromInt(IEEEFloat64Params.bias - IEEEFloat32Params.bias)

  (* convert a 4-byte Word8Vector that represents a 32-bit IEEE float to a 8-byte
   * Word8Vector that represents the corresponding 64-bit float.  The conversion
   * is as follows:
   *
   *    The exact bit representation of the float is laid out with the
   *    corresponding bitwise representation of a double:  the sign
   *    bit is copied over, the exponent is encoded in the larger width,
   *    and the 23 bits of significand fills in the top 23 bits of significand
   *    in the double.  A double has 52 bits of significand, so this means
   *    that the last 29 bits of significand will always be ignored.  As an
   *    error-detection measure, the IR parser requires them to be zero.
   *
   *            - John McCall's message on the LLVM developer mailing list (2011)
   *              https://groups.google.com/d/msg/llvm-dev/IlqV3TbSk6M/27dAggZOMb0J
   *)
    fun to64 v = let
        (* first convert the vector to a Word32 for easy access to bits *)
          val bits = P32.subVec(v, 0)
        (* mask out components of the 32-bit representation *)
          val sign = W32.>>(W32.andb(bits, 0wx80000000), 0w31)
          val exp32 = W32.>>(W32.andb(bits, 0wx7F800000), 0w23)
          val man32 = W32.andb(bits, 0wx007FFFFF)
        (* convert the SP exponent to DP *)
          val exp64 = if (exp32 = 0w0) then 0w0         (* zero *)
                else if (exp32 = 0w255) then 0w2047     (* NaN/Inf *)
                else exp32 + biasOffset                 (* not a special exponent *)
        (* array to build the result *)
          val res = W8A.array(8, 0w0)
          in
          (* fill high 32 bits of result *)
            P32.update(res, 0, sign ++ W32.<<(exp64, 0w20) ++ W32.>>(man32, 0w3));
          (* construct low 32 bits of result *)
            P32.update(res, 1, W32.<<(man32, 0w29));
          (* convert to a vector for the result *)
            W8A.toVector res
          end

  (* the number of bits in the literal representation *)
    val width = 32

    val classify = F32ToBits.classify

  (* convert a floating-point literal to its IEEE binary representation; we also
   * return the IEEE classification of the value.  This function raises the
   * Overflow exception when the literal is too large to represent.
   *)
    fun toBits lit = let
          val (v, cls) = F32ToBits.toBits lit
          in
            (to64 v, cls)
          end

(*
    val fromBits : Word8Vector.vector -> FloatLit.t
*)

    fun zero isNeg = to64(F32ToBits.zero isNeg)
    val negInf = to64(F32ToBits.negInf)
    val posInf = to64(F32ToBits.posInf)
    val quietNaN = to64(F32ToBits.quietNaN)

  end

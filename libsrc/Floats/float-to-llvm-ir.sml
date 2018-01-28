(* float-to-llvm-ir.sml
 *
 * This module supports converting float litearals to hexidecimal
 * strings that are suitable for use as constants in an LLVM
 * assembler file.
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

structure FloatToLLVMIR : sig

    exception InvalidWidth

  (* convert a float literal of the specified width to an LLVM float constant
   * (represented as a string).  Raises InvalidWidth if the width is one of
   * the supported formats (16, 32, 64, or 128).
   *)
    val toString : FloatLit.t * int -> string

  end = struct

    structure F16ToBits = Float16ToLLVM
    structure F32ToBits = Float32ToLLVM
    structure F64ToBits = FloatToBitsFn (IEEEFloat64Params)
    structure F128ToBits = FloatToBitsFn (IEEEFloat128Params)

    exception InvalidWidth

    val byte2s = (StringCvt.padLeft #"0" 2) o Word8.fmt StringCvt.HEX

    fun fmt (prefix, (bytes, _)) = let
          fun fmtByte (b, digits) = byte2s b :: digits
          in
            String.concat (prefix :: Word8Vector.foldr fmtByte [] bytes)
          end

    fun toString (lit,  16) = fmt ("0xH", F16ToBits.toBits lit)
      | toString (lit,  32) = fmt ("0x", F32ToBits.toBits lit)
      | toString (lit,  64) = fmt ("0x", F64ToBits.toBits lit)
      | toString (lit, 128) = fmt ("0xL", F128ToBits.toBits lit)
      | toString _ = raise InvalidWidth

  end

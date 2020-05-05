(* log.sml
 *
 * Support for logging internal messages to a log file.
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
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

structure Log : sig

  (* is logging enabled? *)
    val enabled : unit -> bool

  (* initialize logging to the specified file name; use "-" as the file name to
   * direct output to stdout.
   *)
    val init : string -> unit

  (* get the name of the log file; returns the empty string when logging is not
   * enabled and "-" for stdout.
   *)
    val filename : unit -> string

  (* terminate logging and close the log file *)
    val finish : unit -> unit

  (* return the output stream being used for logging.  This function raises
   * the `Fail` exception when logging has not been initialized.
   *)
    val outstream : unit -> TextIO.outstream

  (* output a message to the log; is a no-op if logging is not enabled *)
    val msg : string list -> unit

  (* if logging is enabled, evaluate the function and output its result to the log *)
    val msg' : (unit -> string list) -> unit

  (* `after { dumpCtl, checkCtl, output, checkIR, fail } (phase, ir)` supports
   * conditional dumping and checking of `ir`.  The IR `ir` is output to the
   * log using the `output` function if either the `dumpCtl` is set or
   * the `checkCtl` is set and `checkIR` returns `true`.  In addition, if
   * `checkIR` returns true, then the function `fail` is called with an
   * error message.
   *)
    val after : {
            dumpCtl : bool Controls.control,
            checkCtl : bool Controls.control,
            output : TextIO.outstream * string * 'prog -> unit,
            checkIR : string * 'prog -> bool,
	    fail : string -> unit
          } -> string * 'prog -> 'prog

  (* report timing to the log file *)
    val reportTiming : PhaseTimer.t -> unit

  end = struct

    val enabledFlg = ref false
    val logStrm : TextIO.outstream option ref = ref NONE
    val logFile = ref ""

    fun enabled () = !enabledFlg

    fun initStrm outS = (
	(* turn off buffering *)
	  TextIO.StreamIO.setBufferMode (TextIO.getOutstream outS, IO.NO_BUF);
	  logStrm := SOME outS)

    fun init file = (case !logStrm
           of NONE => (
		enabledFlg := true;
		if file = "-"
		  then (
		    initStrm TextIO.stdOut;
		    logFile := "<stdout>")
		  else (
		    initStrm (TextIO.openOut file);
		    logFile := file))
            | SOME strm => raise Fail "multiple initialization of log file"
          (* end case *))

    fun filename () = !logFile

    fun finish () = (case !logStrm
	   of SOME strm => (
		if !logFile <> "<stdout>" then TextIO.closeOut strm else ();
		enabledFlg := false;
		logFile := "";
		logStrm := NONE)
	    | NONE => ()
	  (* end case *))

    fun outstream () = (case !logStrm
           of NONE => raise Fail "log file is not initialized"
            | SOME outS => outS
          (* end case *))

    fun msg s = if !enabledFlg then TextIO.output(outstream(), String.concat s) else ()

    fun msg' msgFn = if !enabledFlg
	  then TextIO.output(outstream(), String.concat(msgFn()))
	  else ()

    fun after {dumpCtl, checkCtl, output, checkIR, fail} (phase, prog) = let
          fun dump () = output(outstream(), "after "^phase, prog)
          in
            if Controls.get dumpCtl
              then dump()
              else ();
            if Controls.get checkCtl andalso checkIR("after " ^ phase, prog)
              then (
                if not(Controls.get dumpCtl)  (* avoid duplication *)
                  then dump()
                  else ();
                fail (concat [
                    "***** Internal error after ", phase, ": see ",
		    filename(), " for details\n"
		  ]))
              else ();
	    prog
          end

    fun reportTiming timer = PhaseTimer.report (outstream(), timer)

  end


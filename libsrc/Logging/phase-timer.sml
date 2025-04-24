(* phase-timer.sml
 *
 * Support for timing compiler phases with nesting (similar to the idea of "cost centers").
 *
 * COPYRIGHT (c) 2020 John Reppy (https://cs.uchicago.edu/~jhr)
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

structure PhaseTimer : sig

    type t

  (* create a new top-level timer with the given name *)
    val newTimer : string -> t

  (* create a sub-phase timer with the given name.  Any time measured by this timer
   * will also be assigned to the ancestor of this timer.
   *)
    val newPhase : t * string -> t

  (* start a timer; note that the starting and stopping of timers should respect the
   * nesting of the timers.
   *)
    val start : t -> unit

  (* stop/pause a timer *)
    val stop : t -> unit

  (* wrap a function with a timer; i.e., the timer is started when the function is
   * called and stopped when it returns (or raises an exception).
   *)
    val withTimer : t -> ('a -> 'b) -> 'a -> 'b

  (* generate a report about the timers.  The format of the report is controled
   * by the flags, which have the following meanings:
   *
   *   prefix           -- a prefix prepended to every line of the report
   *   wid              -- a desired line width (including prefix).  The output
   *                       will be at least this wide, but may be wider.
   *   sepChr           -- a character used as the separator between the name
   *                       and values
   *   noZeros          -- if true, counters with zero value are omitted
   *)
    val fmtReport : {
            prefix : string,
            wid : int,
            sepChr : char,
            noZeros : bool
          } -> TextIO.outstream * t -> unit

  (* equivalent to `fmtReport {prefix = "", wid = 60, sepChr = #".", noZeros = false}` *)
    val report : TextIO.outstream * t -> unit

  (* return a JSON object that captures the values of the timers. *)
    val toJSON : t -> JSON.value

  end = struct

    datatype t = T of {
        parent : t option,              (* optional parent of this timer *)
        label : string,                 (* name of the timer *)
        start : Time.time option ref,   (* SOME t when on, otherwise NONE *)
        tot : Time.time ref,            (* total accumulated time for the timer *)
        childTot : Time.time ref,       (* time accumulated by its immediate kids *)
        children : t list ref           (* list of kids *)
      }

    fun newTimer l = T{
          parent = NONE,
          label = l,
          start = ref NONE,
          tot = ref Time.zeroTime,
          childTot = ref Time.zeroTime,
          children = ref []
        }

    fun newPhase (timer as T{children, ...}, l) = let
          val newT = T{
                  parent = SOME timer,
                  label = l,
                  start = ref NONE,
                  tot = ref Time.zeroTime,
                  childTot = ref Time.zeroTime,
                  children = ref []
                }
          in
            children := newT :: !children;
            newT
          end

    fun start (T{label, start, parent, ...}) = (case !start
           of NONE => (
                start := SOME(Time.now());
                case parent
                 of SOME(T{start=ref NONE, label=lab, ...}) => raise Fail(concat[
                        "start(", label, "): parent (", lab, ") is not running"
                      ])
                  | _ => ())
            | SOME _ => ()
          (* end case *))

    fun stop (T{label, parent, start, tot, ...}) = (case !start
           of SOME t0 => let
                val t = Time.-(Time.now(), t0)
                in
                  start := NONE;
                  tot := Time.+(!tot, t);
                  case parent
                   of SOME(T{childTot, ...}) => childTot := Time.+(!childTot, t)
                    | _ => ()
                  (* end case *)
                end
            | NONE => ()
          (* end case *))

    fun withTimer timer f x = let
          val () = start timer
          val y = (f x) handle ex => (stop timer; raise ex)
          in
            stop timer;
            y
          end

    fun fmtReport {prefix, wid, sepChr, noZeros} (outS, timer) = let
          fun pr s = TextIO.output(outS, s)
        (* create a string by repeating a character n times *)
          fun repeat (c, n) = CharVector.tabulate(n, Fn.const c)
          val leftPad = StringCvt.padLeft #" "
          fun time2s t = leftPad 7 (Time.fmt 3 t)
        (* gather the lines for the report *)
          val lns = let
                fun walk depth (T{label, children, tot, childTot, ...}, lns) = let
                      val label = leftPad (2 * depth + size label) label
                      val t1 = let
                            val t = Time.-(!tot, !childTot)
                          (* avoid negative times because of non-monotonic clocks *)
                            val t = if Time.<(t, Time.zeroTime)
                                  then Time.zeroTime
                                  else t
                            in
                              time2s t
                            end
                      val t2 = time2s (!tot)
                      in
                      (* NOTE: we use foldl to reverse the order of the kids *)
                        (label, t1, t2) ::
                          List.foldl
                            (walk (depth + 1))
                              lns
                                (!children)
                      end
                in
                  walk 0 (timer, [])
                end
        (* determine maximum field widths *)
          fun mx (s, m) = Int.max(size s, m)
          val (max1, max2, max3) = List.foldl
                (fn ((s1, s2, s3), (m1, m2, m3)) => (mx(s1, m1), mx(s2, m2), mx(s2, m2)))
                  (0, 0, 0)
                    lns
        (* determine the separator width *)
          val sepWid = wid - size prefix - (max1 + 1) - (max2 + 1) - 2 - max3
        (* the right padding for the label *)
          val pad1 = StringCvt.padRight sepChr (max1 + 1)
          fun fmt1 name = pad1 (name ^ " ")
        (* the minimum seperator is " .... " *)
          val sep = repeat (sepChr, sepWid - 2)
        (* print a line *)
          fun prLn (label, t1, t2) = pr (concat [
                  prefix, fmt1 label, sep, "  ", t1, "   ", t2, "\n"
                ])
        (* center a string in a field of the given width *)
          fun center (s, wid) = let
                val padding = wid - String.size s
                val lPad = padding div 2
                val rPad = padding - lPad
                in
                  if padding < 0 then s
                    else concat[repeat(#" ", lPad), s, repeat(#" ", rPad)]
                end
          in
            pr prefix;
            pr (center ("Phase", max1 + sepWid));
            pr " "; pr(center ("Exclusive", max2 + 2));
            pr "  "; pr(center ("Total", max3 + 1));
            pr "\n";
            List.app prLn lns
          end

(*
    fun report (outS, timer) = let
          fun pr s = TextIO.output(outS, s)
        (* create a string by repeating a character n times *)
          fun repeat (c, n) = CharVector.tabulate(n, Fn.const c)
        (* figure out the length of the longest label in the tree and the depth of the tree *)
          val (maxLabelLen, depth) = let
                fun walk (T{label, children, ...}, maxLen, depth) = let
                      fun doChild (timer, (maxLen, depth)) = let
                            val (l, d) = walk (timer, maxLen, depth)
                            in
                              (Int.max(maxLen, l), Int.max(depth, d))
                            end
                      in
                        List.foldl
                          doChild
                            (Int.max(size label, maxLen), depth+1)
                              (!children)
                      end
                in
                  walk (timer, 0, 0)
                end
          val labelWid = maxLabelLen + 2*depth + 4
        (* display a report line *)
          fun display (indent, T{label, tot, childTot, children, ...}) = let
                fun prTime t = pr(StringCvt.padLeft #" " 7 (Time.fmt 3 t))
                in
                  pr(repeat (#" ", indent));
                  pr(StringCvt.padRight #"." (labelWid+4-indent) (label^" "));
                  pr " "; prTime (Time.-(!tot, !childTot));
                  pr "   "; prTime (!tot); pr "\n";
                  List.app (fn t => display(indent+2, t)) (List.rev (!children))
                end
          fun center (s, wid) = let
                val padding = wid - String.size s
                val lPad = padding div 2
                val rPad = padding - lPad
                in
                  if padding < 0 then s
                    else concat[repeat(#" ", lPad), s, repeat(#" ", rPad)]
                end
          in
            pr (center ("Phase", labelWid + 2));
            pr "  "; pr(center ("Exclusive", 9));
            pr "  "; pr(center ("Total", 9));
            pr "\n";
            display (2, timer)
          end
*)

    val report = fmtReport {prefix = "", wid = 72, sepChr = #".", noZeros = false}

    fun toJSON timer = let
          fun timeToJSON t = JSON.FLOAT(Time.toReal t)
          fun timerToJSON (T{label, tot, childTot, children, ...}) = let
                val fields = if null(!children)
                      then []
                      else [("kids", JSON.ARRAY(List.revMap timerToJSON (!children)))]
                val exclT = let
                      val t = Time.-(!tot, !childTot)
                      in
                        if Time.<(t, Time.zeroTime)
                          then Time.zeroTime
                          else t
                      end
                val fields = ("label", JSON.STRING label) ::
                      ("total", timeToJSON (!tot)) ::
                      ("exclusive", timeToJSON exclT) ::
                      fields
                in
                  JSON.OBJECT fields
                end
          in
            timerToJSON timer
          end

  end

(* phase-timer.sml
 *
 * Support for timing compiler phases with nesting (similar to the idea of "cost centers").
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

    val report : TextIO.outstream * t -> unit

  end = struct

    datatype t = T of {
        parent : t option,              (* optional parent of this timer *)
        label : string,                 (* name of the timer *)
        start : Time.time option ref,   (* SOME t when on, otherwise NONE *)
        tot : Time.time ref,            (* total accumulated time for the timer *)
        childTot : Time.time ref,       (* time accumulated by its kids *)
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
                 of SOME(T{start=ref NONE, ...}) => raise Fail(concat[
                        "start(", label, "): parent is not running"
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

    fun report (outS, timer) = let
          fun pr s = TextIO.output(outS, s)
        (* create a string by repeating a character n times *)
          fun repeat (c, n) = CharVector.tabulate(n, fn _ => c)
        (* figure out the length of the longest label in the tree and the depth of the tree *)
          val (maxLabelLen, depth) = let
                fun walk (T{label, children, ...}, maxLen, depth) = let
                      fun doChild (timer, (maxLen, depth)) = let
                            val (l, d) = walk (timer, maxLen, depth)
                            in
                              (Int.max(maxLen, l), Int.max(depth, d))
                            end
                      in
                        List.foldl doChild (Int.max(size label, maxLen), depth+1) (!children)
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

  end

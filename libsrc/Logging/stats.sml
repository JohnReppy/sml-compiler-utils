(* stats.sml
 *
 * Support for collecting statistics about the internal actions in a
 * compiler.
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

structure Stats : sig

    type group
    type counter

  (* if true, we log individual optimization ticks as they are recorded *)
    val logTicks : bool ref

  (* used to order the groups when reporting results *)
    type priority = int list

    structure Group : sig
      (* a group of counters *)
	type t = group

      (* the group that contains all of the top-level groups *)
	val root : t

      (* `new parent name` creates a new group that is a subgroup of `parent`.
       * The group is given a default priority based on its creation order;
       * _i.e._, the first subgroup is assigned priority `[1]`, the second
       * is assigned `[2]`, _etc_.
       *)
	val new : t -> string -> t

      (* `newWithPri parent (name, pri)` creates a new group that is a
       * subgroup of `parent` and that has the specified priority.
       *)
	val newWithPri : t -> string * priority -> t

      (* return the name of the group.  The root group's name is "" *)
	val name : t -> string

      (* return the group's parent; `NONE` for the root. *)
	val parent : t -> t option

      (* reset the counters in the group (and all subgroups) *)
	val reset : t -> unit

      (* return the sum of the counters in the group (and all subgroups) *)
	val total : t -> int

      (* `hide (grp, true)` marks the group as `hidden` (_i.e._, is does not
       * appear in any reports).  One can "un-hide" a group with the call
       * `hide (grp, false)`.
       *)
	val hide : t * bool -> unit

      (* `hidden grp` returns `true` if the group is currently hidden. *)
	val hidden : t -> bool

      (* `subgroups grp` returns a list of the subgroups of `grp` in the
       * order in which they were created.
       *)
	val subgroups : t -> t list

      (* `subgroupsInOrder grp` returns a list of the subgroups of
       * `grp` in priority order in which they were created.  If the
       * subgroups were all created using `new`, this function behaves
       * the same as `subgroups`.
       *)
	val subgroupsInOrder : t -> t list

      (* `counters grp` returns a list of the counters in the
       * group in the order in which they were created.
       *)
	val counters : t -> counter list

      (* `fmtReport flgs (outS, grp)` generates a report of the counter values
       * to `outS`.  The report's format is controlled by the `flgs` record,
       * which has the following fields:
       *
       *   prefix	-- a prefix prepended to every line of the report
       *   wid		-- a desired line width (including prefix).  The output
       *                   will be at least this wide, but may be wider.
       *   sepChr	-- a character used as the separator between the name
       *		   and values
       *   noZeros	-- if true, counters with zero value are omitted
       *   grpHdrs	-- if true, a header line is printed before each non-empty
       *		   group (non-empty is judged after filtering for zeros
       *		   and hidden subgroups)
       *   fullNames	-- if true, the full names of counters are displayed.
       *)
	val fmtReport : {
		prefix : string,
		wid : int,
		sepChr : char,
		noZeros : bool,
		grpHdrs : bool,
		fullNames : bool
	      } -> TextIO.outstream * t -> unit

      (* `report (outS, grp)` generates a report to the specified output stream.
       * This call is shorthand for
       *
       *	fmtReport
       *	  { prefix = "",
       *            wid = 72, sepChr = #".", noZeros = true,
       *	    grpHdrs=false, fullNames=true
       *          }
       *	    (outS, grp)
       *)
	val report : TextIO.outstream * t -> unit

      (* return a JSON object that captures the values of the counters. *)
	val toJSON : t -> JSON.value

      end

    structure Counter : sig
      (* a counter *)
	type t = counter

      (* `new grp name` allocates a new counter in the specified group. *)
	val new : Group.t -> string -> t

      (* return the counter's name *)
	val name : t -> string

      (* get the full name of a counter, which will be a string of the form
       * `grp1/grp2/.../name`, where `grp1` etc. are the names of the containing
       * groups (not counting the root group),
       *)
	val fullName : t -> string

      (* `tick cntr` adds one to the counter's value.  If the `logTicks` flag
       * is set, then a log message is also generated.
       *)
	val tick : t -> unit

      (* `bump (cntr, n)` adds `n` to the counter.  Note that unlike `tick`, this
       * operation does *not* generate a log message when `logTicks` is set.
       *)
	val bump : t * int -> unit

      (* `value cntr` returns the current value of the counter *)
	val value : t -> int

      (* `reset cntr` resets the counter's value to zero. *)
	val reset : t -> unit

      end

  end = struct

  (* used to order the groups when reporting results *)
    type priority = int list

    datatype group = G of {
	name : string,
	parent : group option,
	pri : priority,
	hidden : bool ref,
	nextId : int ref,
	kids : group list ref,
	counters : counter list ref
      }

    and counter = C of {
	grp : group,
	name : string,
	cnt : int ref
      }

    val logTicks = ref false

  (* get the full name of a counter *)
    fun cntrFullName (C{grp, name, ...}) = let
	  fun get (G{parent=NONE, ...}, l) = String.concat l
	    | get (G{parent=SOME grp, name, ...}, l) =
		get (grp, name :: "/" :: l)
	  in
	    get (grp, [name])
	  end

  (* get the full name of a counter *)
    fun grpFullName (G{parent=NONE, ...}) = ""
      | grpFullName (G{parent=SOME grp, name, ...}) = let
	  fun get (G{parent=NONE, ...}, l) = String.concat l
	    | get (G{parent=SOME grp, name, ...}, l) =
		get (grp, name :: "/" :: l)
	  in
	    get (grp, [name])
	  end

    structure Group =
      struct

	type t = group

	fun mk (name, parent, pri) = G{
		name = name,
		parent = parent,
		pri = pri,
		hidden = ref false,
		nextId = ref 1,
		kids = ref[],
		counters = ref[]
	      }

      (* the group that contains all of the top-level groups *)
	val root = mk ("", NONE, [])

	fun new (parent as G{nextId, kids, ...}) name = let
	      val id = !nextId
	      val grp = mk (name, SOME parent, [id])
	      in
		nextId := id + 1;
		kids := grp :: !kids;
		grp
	      end

	fun newWithPri (parent as G{kids, ...}) (name, pri) = let
	      val grp = mk (name, SOME parent, pri)
	      in
		kids := grp :: !kids;
		grp
	      end

	fun name (G{name, ...}) = name
	fun parent (G{parent, ...}) = parent

	fun reset (G{kids, counters, ...}) = (
	      List.app reset (!kids);
	      List.app (fn (C{cnt, ...}) => cnt := 0) (!counters))

	fun total grp = let
	      fun sum (G{kids, counters, ...}, n) =
		    List.foldl sum
		      (List.foldl (fn (C{cnt, ...}, m) => m + !cnt) n (!counters))
			(!kids)
	      in
		sum (grp, 0)
	      end

	fun hide (G{hidden, ...}, b) = hidden := b
	fun hidden (G{hidden, ...}) = !hidden

      (* sort a list of groups into priority order *)
	val sort = let
	      fun gt (G{pri=p1, ...}, G{pri=p2, ...}) = (
		    case List.collate Int.compare (p1, p2)
		     of GREATER => true
		      | _ => false
		    (* end case *))
	      in
		ListMergeSort.sort gt
	      end

	fun subgroups (G{kids, ...}) = List.rev (!kids)
	fun subgroupsInOrder (G{kids, ...}) = sort (!kids)
	fun counters (G{counters, ...}) = List.rev (!counters)

	fun fmtReport {prefix, wid, sepChr, noZeros, grpHdrs, fullNames} (outS, grp) = let
	      fun itos n = if (n < 0) then "-" ^ Int.toString(~n) else Int.toString n
	      fun getCounters (cntrs, lns) = let
		    fun get (cntr as C{name, cnt, ...}, lns) = (
			  case (noZeros, fullNames, !cnt)
			   of (true, _, 0) => lns
			    | (_, false, n) => (name, itos n) :: lns
			    | (_, true, n) => (cntrFullName cntr, itos n) :: lns
			  (* end case *))
		    in
		    (* Note: the list of counters is in reverse order, so using
		     * foldl here will return the result in creation order.
		     *)
		      List.foldl get lns cntrs
		    end
	    (* we walk the group tree and gather up a list of output lines *)
	      fun getLines (grp as G{name, kids, counters, hidden, ...}, lns') =
		    if !hidden
		      then lns'
		      else let
		      (* lines for the subgroups *)
			val lns = List.foldr getLines [] (sort (!kids))
		      (* lines for the counters are prepended *)
			val lns = getCounters (!counters, lns)
			in
			  case (grpHdrs, lns)
			   of (true, []) => lns'
			    | (true, _) => if fullNames
				then (grpFullName grp, itos(total grp)) :: lns @ lns'
				else (name, itos(total grp)) :: lns @ lns'
			    | _ => lns @ lns'
			  (* end case *)
			end
	      val lns = getLines (grp, [])
	    (* compute maximum widths *)
	      val (max1, max2) = List.foldl
		    (fn ((s1, s2), (m1, m2)) =>
			  (Int.max(size s1, m1), Int.max(size s2, m2)))
		      (0, 0)
			lns
	    (* the right padding for the name *)
	      val pad1 = StringCvt.padRight sepChr (max1 + 1)
	      fun fmt1 name = pad1 (name ^ " ")
	    (* the left padding for the value *)
	      val pad2 = StringCvt.padLeft sepChr (max2 + 1)
	      fun fmt2 value = pad2 (" " ^ value)
	    (* the minimum seperator is " .... " *)
	      val sepWid = Int.max (6, wid - size prefix - max1 - max2)
	      val sep = CharVector.tabulate (sepWid - 2, Fn.const sepChr)
	    (* print a line of the report *)
	      fun prLn (name, value) = TextIO.output(outS, String.concat[
		      prefix, fmt1 name, sep, fmt2 value, "\n"
		    ])
	      in
		List.app prLn lns
	      end

	val report = fmtReport {
		prefix="", wid = 72, sepChr = #".",
		noZeros=true, grpHdrs=false, fullNames=true
	      }

	fun toJSON grp = let
	      fun intToJSON i = JSON.INT(IntInf.fromInt i)
	      fun priToJSON p = JSON.ARRAY(List.map intToJSON p)
	      fun grpToJSON (G{pri, kids, counters, ...}) = let
		    val fields = List.foldr
			  (fn (C{name, cnt, ...}, flds) => (name, intToJSON(!cnt)) :: flds)
			    [] (!counters)
		    val fields = List.foldr
			  (fn (g, flds) => (name g, grpToJSON g) :: flds)
			    fields
			      (sort (List.filter (not o hidden) (!kids)))
		    val fields = ("priority", priToJSON pri) :: fields
		    in
		      JSON.OBJECT fields
		    end
	      in
		if hidden grp then JSON.NULL else grpToJSON grp
	      end

      end

    structure Counter =
      struct

	type t = counter

	fun new (grp as G{counters, ...}) name = let
	      val cntr = C{grp = grp, name = name, cnt = ref 0}
	      in
		counters := cntr :: !counters;
		cntr
	      end

	fun name (C{name, ...}) = name

	val fullName = cntrFullName

	fun tick (cntr as C{cnt, ...}) = (
	      if !logTicks
		then Log.msg ["++ ", fullName cntr, "\n"]
		else ();
	      cnt := !cnt + 1)

	fun bump (C{cnt, ...}, n) = cnt := !cnt + n

	fun value (C{cnt, ...}) = !cnt

	fun reset (C{cnt, ...}) = cnt := 0

      end

  end

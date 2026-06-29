(* all.sml -- all sources for logic *)
local
(******************** util/bmark.sig ********************)
(* bmark.sig
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

signature BMARK =
  sig
(* TODO: add some form of benchmark description *)

    (* the short name for the benchmark *)
    val name : string

    (* run the benchmark program for timing purposes (no output) *)
    val doit : unit -> unit

    (* run the benchmark program and direct its output to the Log output
     * (see `log.sml`).  This function can be used to verify that the
     * benchmark is producing the expected results.
     *)
    val testit : unit -> unit

    (* a (possibly empty) list of generated result files.  This list is used
     * when testing a benchmark for correctness.
     *)
    val results : string list

  end
(******************** util/log.sml ********************)
(* log.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Text output for benchmark programs.  This mechanism allows generation of output
 * when testing a program and suppression of output when benchmarking.
 *)

structure Log : sig

    (* run a command with output directed to the specified log stream *)
    val withOutput : TextIO.outstream -> (unit -> unit) -> (unit -> unit)

    (* output to the current output *)
    val print : string -> unit
    val say : string list -> unit
    val flush : unit -> unit

    (* support for binary output *)
    structure BinIO : sig
        type outstream
        val openOut : string -> outstream
        val closeOut : outstream -> unit
        val output : outstream * Word8Vector.vector -> unit
        val output1 : outstream * Word8.word -> unit
        val flushOut : outstream -> unit
      end

    (* print an error message to TextIO.stdErr *)
    val error : string list -> unit

  end = struct

    val logS : TextIO.outstream option ref = ref NONE

    fun testing () = isSome(! logS)

    fun withOutput outS f () = (
          logS := SOME outS;
          f() handle ex => (logS := NONE; raise ex);
          logS := NONE)

    fun print s = (case !logS
           of SOME outS => TextIO.output(outS, s)
            | NONE => ()
          (* end case *))

    fun outputList (outS, []) = ()
      | outputList (outS, s::r) = (TextIO.output(outS, s); outputList(outS, r))

    fun say msg = (case !logS
           of SOME outS => outputList (outS, msg)
            | NONE => ()
          (* end case *))

    fun flush () = (case !logS
	   of SOME outS => TextIO.flushOut outS
	    | _ => ()
	  (* end case *))

    structure BinIO =
      struct
        type outstream = BinIO.outstream option
        fun openOut file = if testing() then SOME(BinIO.openOut file) else NONE
        fun closeOut (SOME outS) = BinIO.closeOut outS
          | closeOut NONE = ()
        fun output (SOME outS, data) = BinIO.output(outS, data)
          | output (NONE, _) = ()
        fun output1 (SOME outS, byte) = BinIO.output1(outS, byte)
          | output1 (NONE, _) = ()
        fun flushOut (SOME outS) = BinIO.flushOut outS
          | flushOut NONE = ()
      end

    fun error msg = outputList (TextIO.stdErr, msg)

  end
(******************** term.sml ********************)
(* term.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Term =
struct
  datatype term
    = STR of string * term list
    | INT of int
    | CON of string
    | REF of term option ref

  exception BadArg of string
end;
(******************** trail.sml ********************)
(* trail.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Trail =
struct
  local
      open Term
      val global_trail = ref (nil : term option ref list)
      val trail_counter = ref 0
  in
      fun unwind_trail (0, tr) = tr
	| unwind_trail (n, r::tr) =
	  ( r := NONE ; unwind_trail (n-1, tr) )
	| unwind_trail (_, nil) =
	  raise BadArg "unwind_trail"

      fun reset_trail () = ( global_trail := nil )

      fun trail func =
	  let
	      val tc0 = !trail_counter
	  in
	      ( func () ;
	       global_trail :=
	         unwind_trail (!trail_counter-tc0, !global_trail) ;
	       trail_counter := tc0 )
	  end

      fun bind (r, t) =
	  ( r := SOME t ;
	   global_trail := r::(!global_trail) ;
	   trail_counter := !trail_counter+1 )
  end (* local *)
end; (* Trail *)

(******************** unify.sml ********************)
(* unify.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Unify =
struct
  local
    open Term Trail
    fun same_ref (r, REF(r')) = (r = r')
      | same_ref _ = false

    fun occurs_check r t =
	let
	    fun oc (STR(_,ts)) = ocs ts
	      | oc (REF(r')) =
		(case !r' of
		     SOME(s) => oc s
		   | _ => r <> r')
	      | oc (CON _) = true
	      | oc (INT _) = true
	    and ocs nil = true
	      | ocs (t::ts) = oc t andalso ocs ts
	in
	    oc t
	end
    fun deref (t as (REF(x))) =
	(case !x of
	     SOME(s) => deref s
	   | _ => t)
      | deref t = t
    fun unify' (REF(r), t) sc = unify_REF (r,t) sc
      | unify' (s, REF(r)) sc = unify_REF (r,s) sc
      | unify' (STR(f,ts), STR(g,ss)) sc =
	if (f = g)
	    then unifys (ts,ss) sc
	else ()
      | unify' (CON(f), CON(g)) sc =
	if (f = g) then
	    sc ()
	else
	    ()
      | unify' (INT(f), INT(g)) sc =
	if (f = g) then
	    sc ()
	else
	    ()
      | unify' (_, _) sc = ()
    and unifys (nil, nil) sc = sc ()
      | unifys (t::ts, s::ss) sc =
	unify' (deref(t), deref(s))
	(fn () => unifys (ts, ss) sc)
      | unifys _ sc = ()
    and unify_REF (r, t) sc =
	if same_ref (r, t)
	    then sc ()
	else if occurs_check r t
		 then ( bind(r, t) ; sc () )
	     else ()
  in
    val deref = deref
    fun unify (s, t) = unify' (deref(s), deref(t))
  end (* local *)
end; (* Unify *)

(******************** data.sml ********************)
(* data.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Data =
struct
  local
    open Term Trail Unify
    val cons_s = "cons"
    val x_s = "x"
    val nil_s = "nil"
    val o_s = "o"
    val s_s = "s"
    val CON_o_s = CON(o_s)
    val CON_nil_s = CON(nil_s)
    val CON_x_s = CON(x_s)
  in
      fun exists sc = sc (REF(ref(NONE)))

fun move_horiz (T_1, T_2) sc =
(
trail (fn () =>
(
trail (fn () =>
(
trail (fn () =>
(
trail (fn () =>
(
trail (fn () =>
(
trail (fn () =>
(
trail (fn () =>
(
trail (fn () =>
(
trail (fn () =>
(
trail (fn () =>
(
trail (fn () =>
exists (fn T =>
exists (fn TT =>
unify (T_1, STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, T])])]), TT])) (fn () =>
unify (T_2, STR(cons_s, [STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, T])])]), TT])) (fn () =>
sc ())))))
;
exists (fn P1 =>
exists (fn P5 =>
exists (fn TT =>
unify (T_1, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, STR(cons_s, [P5, CON_nil_s])])])])]), TT])) (fn () =>
unify (T_2, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, STR(cons_s, [P5, CON_nil_s])])])])]), TT])) (fn () =>
sc ())))))
))
;
exists (fn P1 =>
exists (fn P2 =>
exists (fn TT =>
unify (T_1, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [P2, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, CON_nil_s])])])])]), TT])) (fn () =>
unify (T_2, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [P2, STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, CON_nil_s])])])])]), TT])) (fn () =>
sc ())))))
))
;
exists (fn L1 =>
exists (fn P4 =>
exists (fn TT =>
unify (T_1, STR(cons_s, [L1, STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, STR(cons_s, [P4, CON_nil_s])])])]), TT])])) (fn () =>
unify (T_2, STR(cons_s, [L1, STR(cons_s, [STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, STR(cons_s, [P4, CON_nil_s])])])]), TT])])) (fn () =>
sc ())))))
))
;
exists (fn L1 =>
exists (fn P1 =>
exists (fn TT =>
unify (T_1, STR(cons_s, [L1, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, CON_nil_s])])])]), TT])])) (fn () =>
unify (T_2, STR(cons_s, [L1, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, CON_nil_s])])])]), TT])])) (fn () =>
sc ())))))
))
;
exists (fn L1 =>
exists (fn L2 =>
exists (fn TT =>
unify (T_1, STR(cons_s, [L1, STR(cons_s, [L2, STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, CON_nil_s])])]), TT])])])) (fn () =>
unify (T_2, STR(cons_s, [L1, STR(cons_s, [L2, STR(cons_s, [STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, CON_nil_s])])]), TT])])])) (fn () =>
sc ())))))
))
;
exists (fn T =>
exists (fn TT =>
unify (T_1, STR(cons_s, [STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, T])])]), TT])) (fn () =>
unify (T_2, STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, T])])]), TT])) (fn () =>
sc ()))))
))
;
exists (fn P1 =>
exists (fn P5 =>
exists (fn TT =>
unify (T_1, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [P5, CON_nil_s])])])])]), TT])) (fn () =>
unify (T_2, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, STR(cons_s, [P5, CON_nil_s])])])])]), TT])) (fn () =>
sc ())))))
))
;
exists (fn P1 =>
exists (fn P2 =>
exists (fn TT =>
unify (T_1, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [P2, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, CON_nil_s])])])])]), TT])) (fn () =>
unify (T_2, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [P2, STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, CON_nil_s])])])])]), TT])) (fn () =>
sc ())))))
))
;
exists (fn L1 =>
exists (fn P4 =>
exists (fn TT =>
unify (T_1, STR(cons_s, [L1, STR(cons_s, [STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [P4, CON_nil_s])])])]), TT])])) (fn () =>
unify (T_2, STR(cons_s, [L1, STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, STR(cons_s, [P4, CON_nil_s])])])]), TT])])) (fn () =>
sc ())))))
))
;
exists (fn L1 =>
exists (fn P1 =>
exists (fn TT =>
unify (T_1, STR(cons_s, [L1, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, CON_nil_s])])])]), TT])])) (fn () =>
unify (T_2, STR(cons_s, [L1, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, CON_nil_s])])])]), TT])])) (fn () =>
sc ())))))
))
;
exists (fn L1 =>
exists (fn L2 =>
exists (fn TT =>
unify (T_1, STR(cons_s, [L1, STR(cons_s, [L2, STR(cons_s, [STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, CON_nil_s])])]), TT])])])) (fn () =>
unify (T_2, STR(cons_s, [L1, STR(cons_s, [L2, STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, CON_nil_s])])]), TT])])])) (fn () =>
sc ())))))
)

and rotate (T_1, T_2) sc =
exists (fn P11 =>
exists (fn P12 =>
exists (fn P13 =>
exists (fn P14 =>
exists (fn P15 =>
exists (fn P21 =>
exists (fn P22 =>
exists (fn P23 =>
exists (fn P24 =>
exists (fn P31 =>
exists (fn P32 =>
exists (fn P33 =>
exists (fn P41 =>
exists (fn P42 =>
exists (fn P51 =>
unify (T_1, STR(cons_s, [STR(cons_s, [P11, STR(cons_s, [P12, STR(cons_s, [P13, STR(cons_s, [P14, STR(cons_s, [P15, CON_nil_s])])])])]), STR(cons_s, [STR(cons_s, [P21, STR(cons_s, [P22, STR(cons_s, [P23, STR(cons_s, [P24, CON_nil_s])])])]), STR(cons_s, [STR(cons_s, [P31, STR(cons_s, [P32, STR(cons_s, [P33, CON_nil_s])])]), STR(cons_s, [STR(cons_s, [P41, STR(cons_s, [P42, CON_nil_s])]), STR(cons_s, [STR(cons_s, [P51, CON_nil_s]), CON_nil_s])])])])])) (fn () =>
unify (T_2, STR(cons_s, [STR(cons_s, [P51, STR(cons_s, [P41, STR(cons_s, [P31, STR(cons_s, [P21, STR(cons_s, [P11, CON_nil_s])])])])]), STR(cons_s, [STR(cons_s, [P42, STR(cons_s, [P32, STR(cons_s, [P22, STR(cons_s, [P12, CON_nil_s])])])]), STR(cons_s, [STR(cons_s, [P33, STR(cons_s, [P23, STR(cons_s, [P13, CON_nil_s])])]), STR(cons_s, [STR(cons_s, [P24, STR(cons_s, [P14, CON_nil_s])]), STR(cons_s, [STR(cons_s, [P15, CON_nil_s]), CON_nil_s])])])])])) (fn () =>
sc ())))))))))))))))))

and move (T_1, T_2) sc =
(
trail (fn () =>
(
trail (fn () =>
exists (fn X =>
exists (fn Y =>
unify (T_1, X) (fn () =>
unify (T_2, Y) (fn () =>
move_horiz (X, Y) sc)))))
;
exists (fn X =>
exists (fn X1 =>
exists (fn Y =>
exists (fn Y1 =>
unify (T_1, X) (fn () =>
unify (T_2, Y) (fn () =>
rotate (X, X1) (fn () =>
move_horiz (X1, Y1) (fn () =>
rotate (Y, Y1) sc))))))))
))
;
exists (fn X =>
exists (fn X1 =>
exists (fn Y =>
exists (fn Y1 =>
unify (T_1, X) (fn () =>
unify (T_2, Y) (fn () =>
rotate (X1, X) (fn () =>
move_horiz (X1, Y1) (fn () =>
rotate (Y1, Y) sc))))))))
)

and solitaire (T_1, T_2, T_3) sc =
(
trail (fn () =>
exists (fn X =>
unify (T_1, X) (fn () =>
unify (T_2, STR(cons_s, [X, CON_nil_s])) (fn () =>
unify (T_3, INT(0)) (fn () =>
sc ())))))
;
exists (fn N =>
exists (fn X =>
exists (fn Y =>
exists (fn Z =>
unify (T_1, X) (fn () =>
unify (T_2, STR(cons_s, [X, Z])) (fn () =>
unify (T_3, STR(s_s, [N])) (fn () =>
move (X, Y) (fn () =>
solitaire (Y, Z, N) sc))))))))
)

and solution1 (T_1) sc =
exists (fn X =>
unify (T_1, X) (fn () =>
solitaire (STR(cons_s, [STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, CON_nil_s])])])])]), STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s,
 CON_nil_s])])])]), STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, CON_nil_s])])]), STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, CON_nil_s])]), STR(cons_s, [STR(cons_s, [CON_x_s, CON_nil_s]), CON_nil_s])])])])])
, X, STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [INT(0)])])])])])])])])])])])])])) sc))

and solution2 (T_1) sc =
exists (fn X =>
unify (T_1, X) (fn () =>
solitaire (STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, CON_nil_s])])])])]), STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s,
 CON_nil_s])])])]), STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, CON_nil_s])])]), STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, CON_nil_s])]), STR(cons_s, [STR(cons_s, [CON_x_s, CON_nil_s]), CON_nil_s])])])])])
, X, STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [INT(0)])])])])])])])])])])])])])) sc))
  end (* local *)
end; (* Data *)
in
(******************** main.sml ********************)
(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "logic"

    val results : string list = []

    exception Done

    fun testit () = Data.exists(fn Z => Data.solution2 Z (fn () => raise Done))
	  handle Done => Log.print "OK\n"

    fun loop n = if n <= 0
	  then ()
          else (
	    Data.exists(fn Z => Data.solution2 Z (fn () => raise Done))
              handle Done => ();
            loop (n - 1))

    fun doit () = loop 60

  end; (* Main *)
end

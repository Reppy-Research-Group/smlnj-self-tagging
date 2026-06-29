(* all.sml -- all sources for pidigits *)
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
(******************** ../BASIS/mono-vector.sig ********************)
(* mono-vector.sig
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Generic interface for monomorphic vector structures.  Note that this
 * includes the various Basis Library proposals.
 *)

signature MONO_VECTOR =
  sig

    type vector
    type elem

    val maxLen : int

  (* vector creation functions *)
    val fromList : elem list -> vector
    val tabulate : int * (int -> elem) -> vector

    val length   : vector -> int
    val sub      : vector * int -> elem
    val concat   : vector list -> vector

    val update : vector * int * elem -> vector

    val appi   : (int * elem -> unit) -> vector -> unit
    val app    : (elem -> unit) -> vector -> unit
    val mapi   : (int * elem -> elem) -> vector -> vector
    val map    : (elem -> elem) -> vector -> vector
    val foldli : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldri : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldl  : (elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldr  : (elem * 'a -> 'a) -> 'a -> vector -> 'a

    val findi  : (int * elem -> bool) -> vector -> (int * elem) option
    val find   : (elem -> bool) -> vector -> elem option
    val exists : (elem -> bool) -> vector -> bool
    val all    : (elem -> bool) -> vector -> bool
    val collate: (elem * elem -> order) -> vector * vector -> order

    (* Basis Library proposal 2015-003 *)
    val toList  : vector -> elem list
    val append  : vector * elem -> vector
    val prepend : elem * vector -> vector

  end
(******************** stream.sml ********************)
(* stream.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Stream =
  struct

    datatype 'a u = Nil | Cons of 'a * 'a t
    withtype 'a t = unit -> 'a u

    fun unfold (f : 'b -> ('a * 'b) option) : 'b -> 'a t = let
          fun loop b () = (case f b
                of NONE => Nil
                 | SOME (x, b) => Cons (x, loop b))
          in
            loop
          end

    fun map (f : 'a -> 'b) : 'a t -> 'b t =
          unfold (fn s => case s () of Nil => NONE | Cons (x, xs) => SOME (f x, xs))

  end
(******************** pi-digits.sml ********************)
(* pi-digits.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure PiDigits : sig

    val pi : IntInf.int Stream.t

  end = struct

    fun stream {
            next : 'b -> 'c,
            safe : 'b -> 'c -> bool,
            prod : 'b -> 'c -> 'b,
            cons : 'b -> 'a -> 'b
          } : 'b -> 'a Stream.t -> 'c Stream.t = let
          fun loop z s () = let
                val y = next z
                in
                  if safe z y
                    then Stream.Cons (y, loop (prod z y) s)
                    else (case s ()
                       of Stream.Nil => Stream.Nil
                        | Stream.Cons (x, xs) => loop (cons z x) xs ())
                end
          in
            loop
          end

    type lft = (IntInf.int * IntInf.int * IntInf.int * IntInf.int)

    val unit : lft = (1,0,0,1)

    fun comp (q,r,s,t) (u,v,w,x) : lft = (q*u+r*w, q*v+r*x, s*u+t*w, s*v+t*x)

    val pi = let
          val init = unit
          val lfts = Stream.map (fn k => (k, 4*k+2, 0, 2*k+1)) (Stream.unfold (fn i => SOME (i, i+1)) 1)
          fun floor_extr (q,r,s,t) x = (q * x + r) div (s * x + t)
          fun next z = floor_extr z 3
          fun safe z n = n = floor_extr z 4
          fun prod z n = comp (10, ~10*n, 0, 1) z
          fun cons z z' = comp z z'
          in
            stream {next = next, safe = safe, prod = prod, cons = cons} init lfts
          end

  end
in
(******************** main.sml ********************)
(* main.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "pidigits"

    val results = []

    fun display n = let
          fun loop (ds, (k, col)) = if k < n
                then let
                  val col = if col = 10
                        then (Log.say["\t:", IntInf.toString k, "\n"]; 1)
                        else col + 1
                  in
                    case ds ()
                     of Stream.Nil => raise Empty
                      | Stream.Cons(d, ds) => (
                          Log.print (IntInf.toString d);
                          loop (ds, (k + 1, col)))
                  end
                else Log.say [
                    "\t:", IntInf.toString k, "\n"
                  ]
          in
            loop (PiDigits.pi, (0, 0))
          end

    fun testit () = display 30

    fun doit () = display 2000

  end
end

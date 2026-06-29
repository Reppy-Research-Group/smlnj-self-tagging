(* all.sml -- all sources for stream-sieve *)
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
(******************** streams.sml ********************)
(* streams.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Streams : sig

    type 'a t

    val make : 'a * (unit -> 'a t) -> 'a t

    val unfold : 'a t -> 'a * 'a t

    val get : 'a t * int -> 'a

    val take : 'a t * int -> 'a list

  end = struct

    datatype 'a t = S of 'a * (unit -> 'a t)

    val make = S

    fun unfold (S(fst, thunk)) = (fst, thunk())

    fun get (strm, n) = if (n < 0)
          then raise Subscript
          else let
            fun lp (0, S(fst, _)) = fst
              | lp (n, S(_, thunk)) = lp (n-1, thunk())
            in
              lp (n, strm)
            end

    fun take (strm, n) = if (n < 0)
          then raise Subscript
          else let
            fun lp (0, _) = []
              | lp (n, S(fst, thunk)) = fst :: lp(n-1, thunk())
            in
              lp (n, strm)
            end

  end
(******************** sieve.sml ********************)
(* sieve.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Sieve : sig

    val primes : int Streams.t

  end = struct

    fun countFromN n = Streams.make (n, fn () => countFromN (n+1))

    fun sift (n, strm) = let
          val (k, strm') = Streams.unfold strm
          in
            if Int.rem(k, n) = 0
              then sift (n, strm')
              else Streams.make(k, fn () => sift (n, strm'))
          end

    (* Sieve of Eratosthenes *)
    fun sieve strm = let
          val (k, strm') = Streams.unfold strm
          in
            Streams.make (k, fn () => sieve (sift (k, strm')))
          end

    val primes = sieve (countFromN 2)

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

    val name = "stream-sieve"

    val results : string list = []

    fun testit () = let
          val res = Streams.take (Sieve.primes, 10)
          in
            if ListPair.allEq (op =) (res, [2,3,5,7,11,13,17,19,23,29])
              then Log.print "OK\n"
              else Log.print "FAIL\n"
          end

    fun doit () = ignore (Streams.get (Sieve.primes, 20000))

  end
end

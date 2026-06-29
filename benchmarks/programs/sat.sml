(* all.sml -- all sources for sat *)
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
in
(******************** main.sml ********************)
(* main.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "sat"

    val results : string list = []

    fun AND [] = true
      | AND (false::_) = false
      | AND (true::bs) = AND bs

    fun OR [] = false
      | OR (true::_) = true
      | OR (false::bs) = OR bs

    fun phi (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = AND [
            OR[x1, x2],
            OR[x1, not x2, not x3],
            OR[x3, x4],
            OR[not x4, x1],
            OR[not x2, not x3],
            OR[x4, x2],
            OR[not x5, x1, x2],
            OR[not x2, not x6],
            OR[not x4, x7]
          ]

    fun checkPhi (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = let
          fun b2s true = "T" | b2s false = "F"
          val res = phi(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
          in
            Log.say [
                "φ(", String.concatWithMap "," b2s [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10],
                ") = ", b2s res, "\n"
              ];
            res
          end

    fun solve () = let
          fun try f = f true orelse f false
          in
            try (fn x1 =>
              try (fn x2 =>
                try (fn x3 =>
                  try (fn x4 =>
                    try (fn x5 =>
                      try (fn x6 =>
                        try (fn x7 =>
                          try (fn x8 =>
                            try (fn x9 =>
                              try (fn x10 =>
                                checkPhi (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)))))))))))
          end

    fun testit () = ignore(solve ())

    fun doit () = let
          fun lp (0, k) = ()
            | lp (n, k) = if solve () then lp (n-1, k+1) else lp (n-1, k)
          in
            lp (1000000, 0)
          end

  end
end

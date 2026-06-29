(* all.sml -- all sources for iter-pidigits *)
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
(******************** pi-digits.sml ********************)
(* pi-digits.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure PiDigits : sig

    val generate : int -> unit

  end = struct

    fun generate nDigits = let
          fun lp (w, n1, n2, d, col, k) = let
                val u = IntInf.quot(n1, d)
                val v = IntInf.quot(n2, d)
                in
                  if (u = v)
                    then let
                      val () = Log.print(IntInf.toString u);
                      val col = col+1
                      in
                        if (col mod 10 = 0)
                          then Log.say["\t:", Int.toString col, "\n"]
                          else ();
                        if (col = nDigits)
                          then ()
                          else let
                            val u = d * (~10 * u)
                            val n1 = 10 * n1 + u
                            val n2 = 10 * n2 + u
                            in
                              lp (w, n1, n2, d, col, k)
                            end
                      end
                    else let
                      val k2 = 2 * k
                      val w = n1 * IntInf.fromInt(k - 1)
                      val n1 = n1 * IntInf.fromInt(k2 - 1) + n2 + n2
                      val n2 = w + n2 * IntInf.fromInt(k + 2)
                      val d = d * IntInf.fromInt(k2 + 1)
                      in
                        lp (w, n1, n2, d, col, k+1)
                      end
                end
          in
            lp (0, 4, 3, 1, 0, 1);
            case (nDigits mod 10)
             of 0 => ()
              | n => Log.say[
                    "\t:", Int.toString nDigits, "\n"
                  ]
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

    fun display n = PiDigits.generate n

    fun testit () = display 30

    fun doit () = display 2000

  end
end

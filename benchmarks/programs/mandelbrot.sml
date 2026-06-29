(* all.sml -- all sources for mandelbrot *)
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
(* mandelbrot.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "mandelbrot"

    val results : string list = []

    val x_base = ~2.0
    val y_base = 1.25
    val side = 2.5

    val sz = 2048
    val maxCount = 1024

    val delta = side / (real sz)

    val sum_iterations = ref 0

    fun loop1 i = if (i >= sz)
	  then ()
	  else let
            val c_im : real = y_base - (delta * real i)
            fun loop2 j = if (j >= sz)
		  then ()
		  else let
		  (* NOTE: older versions of the benchmark had the following
		   * incorrect code:
		    val c_re = x_base * (delta + real_j)
		   *)
                    val c_re = x_base + (delta * real j)
		    fun loop3 (count, z_re : real, z_im : real) = if (count < maxCount)
			  then let
                            val z_re_sq = z_re * z_re
                            val z_im_sq = z_im * z_im
                            in
                              if ((z_re_sq + z_im_sq) > 4.0)
                                then count
                                else let
                                  val z_re_im = (z_re * z_im)
                                  in
                                    loop3 (count+1,
                                      (z_re_sq - z_im_sq) + c_re,
                                       z_re_im + z_re_im + c_im)
                                  end
                            end (* loop3 *)
			  else count
		    val count = loop3 (0, c_re, c_im)
		    in
		      sum_iterations := !sum_iterations + count;
		      loop2 (j+1)
		    end
            in
              loop2 0;
              loop1 (i+1)
            end

    fun doit () = (sum_iterations := 0; loop1 0)

    fun testit () = (
	  sum_iterations := 0;
	  loop1 0;
	  Log.say [Int.toString(!sum_iterations), " iterations\n"])

  end (* Mandelbrot *)
end

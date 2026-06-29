(* all.sml -- all sources for binary-trees *)
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
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "binary-trees"

    datatype tree = Empty | Node of tree * tree

    fun make 0 = Node(Empty, Empty)
      | make d = let val d = d - 1 in Node(make d, make d) end

    fun checksum (Node(Empty, _)) = 1
      | checksum (Node(t1, t2)) = 1 + checksum t1 + checksum t2
      | checksum _ = raise Fail "bad tree"

    fun bmark n = let
          val minDepth = 4
          val maxDepth = Int.max(n, minDepth+2)
          (* stretch tree *)
          val stretchTree = make (maxDepth+1)
          val () = Log.say [
                  "stretch tree of depth ", Int.toString(maxDepth+1),
                  "\t check: ", Int.toString(checksum stretchTree), "\n"
                ]
          (* long lived tree *)
          val longTree = make maxDepth
          fun lp1 depth = if (depth <= maxDepth)
                then let
                  val nIters = Word.toIntX(
                        Word.<<(0w1, Word.fromInt(maxDepth-depth+minDepth)))
                  fun lp2 (i, cs) = if (i <= nIters)
                        then let
                          val tr = make depth
                          in
                            lp2 (i+1, cs + checksum tr)
                          end
                        else cs
                  in
                    Log.say [
                        Int.toString nIters, "\t trees of depth ", Int.toString depth,
                        "\t check: ", Int.toString(lp2 (1, 0)), "\n"
                      ];
                    lp1 (depth + 2)
                  end
                else ()
          in
            lp1 minDepth;
            Log.say [
                "long lived tree of depth ", Int.toString maxDepth,
                "\t check: ", Int.toString(checksum longTree), "\n"
              ]
          end

    fun doit () = bmark 21

    fun testit () = bmark 10

    val results = []

  end
end

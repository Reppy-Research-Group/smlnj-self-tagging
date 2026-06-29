(* all.sml -- all sources for pingpong *)
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
(******************** ../common/cml.sml ********************)
(* cml.sml
 *
 * Simple implementation of CML-like message passing.
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Queue :> sig

    type 'a t

    val new : unit -> 'a t

    val next : 'a t -> 'a option

    val insert : 'a t * 'a -> unit

    val isEmpty : 'a t -> bool

    val clear : 'a t -> unit

  end = struct

    type 'a t = {front : 'a list ref, rear : 'a list ref}

    fun new () = {front = ref [], rear = ref []}

    fun next ({ front, rear } : 'a t) = (case !front
           of [] => (case rev(!rear)
                 of [] => NONE
                  | x::xs => (front := xs; rear := []; SOME x)
                (* end case *))
            | x::xs => (front := xs; SOME x)
          (* end case *))

    fun insert (q : 'a t, x) = (#rear q) := x :: !(#rear q)

    fun isEmpty (q : 'a t) = (case !(#front q)
           of [] => (case !(#rear q) of [] => true | _ => false)
            | _ => false
          (* end case *))

    fun clear ({ front, rear } : 'a t) = (front := nil; rear := nil)

  end

structure CML : sig

    val spawn : (unit -> unit) -> unit

    val yield : unit -> unit

    val exit : unit -> 'a

    val run : ('a -> 'b) * 'a -> 'b

    type 'a chan

    val channel : unit -> 'a chan
    val send : 'a chan * 'a -> unit
    val recv : 'a chan -> 'a

  end = struct

    type 'a cont = 'a SMLofNJ.Cont.cont
    val callcc = SMLofNJ.Cont.callcc
    val throw = SMLofNJ.Cont.throw

    (* the scheduling queue *)
    val readyQ : unit cont Queue.t = Queue.new()

    val topCont : unit cont option ref = ref NONE

    fun exit () = (case !topCont
         of SOME k => (topCont := NONE; throw k ())
          | NONE => (
                Log.error ["\n!!! invalid exit\n"];
                raise Fail "exit")
          (* end case *))

    (* dispatch the next thread *)
    fun dispatch () = (case Queue.next readyQ
         of NONE => (
              Log.error ["\n!!! deadlock\n"];
              exit ())
          | SOME k => throw k ()
          (* end case *))

    fun uncaught exn = (
          Log.error [
              "\n!!! uncaught exception: ", General.exnMessage exn, "\n"
            ];
          dispatch())

    fun spawn f = callcc (fn (retK : unit cont) =>
            (callcc (fn (thrdK : unit cont) => (
                Queue.insert (readyQ, thrdK); (* insert new thread in readyQ *)
                throw retK ())); (* return to parent *)
            (f ()) handle ex => uncaught ex))

    fun yield () = callcc (fn retK => (
          Queue.insert (readyQ, retK);
          dispatch ()))

    val topCont : unit cont option ref = ref NONE

    fun exit () = (case !topCont
         of SOME k => (topCont := NONE; throw k ())
          | NONE => (
              Log.error ["\n!!! invalid exit\n"];
              raise Fail "exit")
          (* end case *))

    (***** run a CML program *****)

    fun run (f, arg) = let
          val res = ref NONE
          in
            Queue.clear readyQ;
            callcc (fn k => (
              topCont := SOME k;
              (res := SOME(f arg))
                handle exn => (topCont := NONE; raise exn)));
            valOf (!res)
          end

    (***** Channels *****)

    type 'a chan = {
        sendQ : ('a * unit cont) Queue.t,
        recvQ : 'a cont Queue.t
      }

    fun channel () = {sendQ = Queue.new(), recvQ = Queue.new()}

    fun send (ch : 'a chan, msg : 'a) = callcc (fn retK => (case Queue.next (#recvQ ch)
           of SOME recvK => (
                Queue.insert (readyQ, retK);
                throw recvK msg)
            | NONE => (
                Queue.insert (#sendQ ch, (msg, retK));
                dispatch())
          (* end case *)))

    fun recv (ch : 'a chan) = callcc (fn retK => (case Queue.next (#sendQ ch)
           of SOME(msg, sendK) => (
                Queue.insert (readyQ, sendK);
                msg)
            | NONE => (
                Queue.insert (#recvQ ch, retK);
                dispatch())
          (* end case *)))

  end
(******************** pingpong.sml ********************)
(* pingpong.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure PingPong : sig

    val run : int -> unit

  end = struct

    fun run n = let
	  val ch = CML.channel()
	  fun ping i = if (i < n)
		then let
		  val _ = CML.send(ch, i)
		  val ack = CML.recv ch
		  in
		    ping ack
		  end
		else ()
	  fun pong () = let
		val msg = CML.recv ch + 1
		in
		  CML.send (ch, msg);
		  if (msg < n) then pong() else ()
		end
	  in
	    CML.spawn pong;
	    ping 0
	  end

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

    val name = "pingpong"

    val results : string list = []

    fun testit outS = (
          CML.run (PingPong.run, 10);
          print "ok\n")

    fun doit () = CML.run (PingPong.run, 100000000)

  end
end

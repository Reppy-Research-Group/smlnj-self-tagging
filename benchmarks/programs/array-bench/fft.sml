(* all.sml -- all sources for fft *)
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
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * The "Fast Fourier Transform" (FFT) benchmark (double precision).
 *)

structure Main : BMARK =
  struct

    val name = "fft64"

    val results : string list = []

    structure A = Real64Array
    structure R = Real64
    structure M = R.Math

    val say = Log.print
    fun say' toS v = Log.print(toS v)

    val printr = say' R.toString
    val printi = say' Int.toString

    val tpi = 2.0 * M.pi

    fun fft px py np = let
          fun find_num_points i m = if i < np then find_num_points (i+i) (m+1) else (i,m)
          val (n,m) = find_num_points 2 1
          in
            if n <> np
              then let
                fun loop i = if i > n
                      then ()
                      else (
                        A.update(px, i, 0.0);
                        A.update(py, i, 0.0);
                        loop (i+1))
                in
                  loop (np+1);
                  say "Use "; printi n; say " point fft\n"
                end
              else ();
            let
            fun loop_k k n2 = if k >= m
                  then ()
                  else let
                    val n4 = n2 div 4
                    val e  = tpi / (R.fromInt n2)
                    fun loop_j j a = if j > n4 then () else
                      let val a3 = 3.0 * a
                          val cc1 = M.cos(a)
                          val ss1 = M.sin(a)
                          val cc3 = M.cos(a3)
                          val ss3 = M.sin(a3)
                          fun loop_is is id = if is >= n then () else
                            let fun loop_i0 i0 = if i0 >= n then () else
                              let val i1 = i0 + n4
                                  val i2 = i1 + n4
                                  val i3 = i2 + n4
                                  val r1 = A.sub(px, i0) - A.sub(px, i2)
                                  val _ = A.update(px, i0, A.sub(px, i0) + A.sub(px, i2))
                                  val r2 = A.sub(px, i1) - A.sub(px, i3)
                                  val _ = A.update(px, i1, A.sub(px, i1) + A.sub(px, i3))
                                  val s1 = A.sub(py, i0) - A.sub(py, i2)
                                  val _ = A.update(py, i0, A.sub(py, i0) + A.sub(py, i2))
                                  val s2 = A.sub(py, i1) - A.sub(py, i3)
                                  val _ = A.update(py, i1, A.sub(py, i1) + A.sub(py, i3))
                                  val s3 = r1 - s2
                                  val r1 = r1 + s2
                                  val s2 = r2 - s1
                                  val r2 = r2 + s1
                                  val _ = A.update(px, i2, r1*cc1 - s2*ss1)
                                  val _ = A.update(py, i2, ~s2*cc1 - r1*ss1)
                                  val _ = A.update(px, i3, s3*cc3 + r2*ss3)
                                  val _ = A.update(py, i3, r2*cc3 - s3*ss3)
                                  in
                                    loop_i0 (i0 + id)
                                  end
                              in
                                loop_i0 is;
                                loop_is (2 * id - n2 + j) (4 * id)
                              end
                            in
                              loop_is j (2 * n2);
                              loop_j (j+1) (e * R.fromInt j)
                            end
                    in
                      loop_j 1 0.0;
                      loop_k (k+1) (n2 div 2)
                    end
            in
              loop_k 1 n
            end;

            (************************************)
            (*  Last stage, length=2 butterfly  *)
            (************************************)
            let fun loop_is is id = if is >= n then () else
              let fun loop_i0 i0 = if i0 > n then () else
                let val i1 = i0 + 1
                    val r1 = A.sub(px, i0)
                    val _ = A.update(px, i0, r1 + A.sub(px, i1))
                    val _ = A.update(px, i1, r1 - A.sub(px, i1))
                    val r1 = A.sub(py, i0)
                    val _ = A.update(py, i0, r1 + A.sub(py, i1))
                    val _ = A.update(py, i1, r1 - A.sub(py, i1))
                in
                  loop_i0 (i0 + id)
                end
              in
                loop_i0 is;
                loop_is (2*id - 1) (4 * id)
              end
            in
              loop_is 1 4
            end;

            (*************************)
            (*  Bit reverse counter  *)
            (*************************)
            let fun loop_i i j = if i >= n then () else
             (if i < j then
               (let val xt = A.sub(px, j)
                in A.update(px, j, A.sub(px, i)); A.update(px, i, xt)
                end;
                let val xt = A.sub(py, j)
                in A.update(py, j, A.sub(py, i)); A.update(py, i, xt)
                end)
              else ();
              let fun loop_k k j =
                        if k < j then loop_k (k div 2) (j-k) else j+k
                  val j' = loop_k (n div 2) j
              in
                loop_i (i+1) j'
              end)
            in
              loop_i 1 1
            end;

            n
          end (* fft *)

    fun test np = let
          val _ = (printi np; say "... ")
          val enp = R.fromInt np
          val npm = (np div 2) - 1
          val pxr = A.array (np+2, 0.0)
          val pxi = A.array (np+2, 0.0)
          val t = M.pi / enp
          val _ = A.update(pxr, 1, (enp - 1.0) * 0.5)
          val _ = A.update(pxi, 1, 0.0)
          val n2 = np  div  2
          val _ = A.update(pxr, n2+1, ~0.5)
          val _ = A.update(pxi, n2+1,  0.0)
          fun loop_i i = if i > npm then () else
            let val j = np - i
                val _ = A.update(pxr, i+1, ~0.5)
                val _ = A.update(pxr, j+1, ~0.5)
                val z = t * R.fromInt i
                val y = ~0.5*(M.cos(z)/M.sin(z))
                val _ = A.update(pxi, i+1,  y)
                val _ = A.update(pxi, j+1, ~y)
            in
              loop_i (i+1)
            end
          val _ = loop_i 1
    (***
          val _ = print "\n"
          fun loop_i i = if i > 15 then () else
            (print i; print "\t";
             print (sub(pxr, i+1)); print "\t";
             print (sub(pxi, i+1)); print "\n"; loop_i (i+1))
          val _ = loop_i 0
    ***)
          val _ = fft pxr pxi np
    (***
          fun loop_i i = if i > 15 then () else
            (print i; print "\t";
             print (sub(pxr, i+1)); print "\t";
             print (sub(pxi, i+1)); print "\n"; loop_i (i+1))
          val _ = loop_i 0
    ***)
          fun loop_i i zr zi kr ki = if i >= np then (zr,zi) else
            let val a = R.abs(A.sub(pxr, i+1) - R.fromInt i)
                val (zr, kr) =
                  if zr < a then (a, i) else (zr, kr)
                val a = R.abs(A.sub(pxi, i+1))
                val (zi, ki) =
                  if zi < a then (a, i) else (zi, ki)
            in
              loop_i (i+1) zr zi kr ki
            end
          val (zr, zi) = loop_i 0 0.0 0.0 0 0
          val zm = if R.abs zr < R.abs zi then zi else zr
          in
            printr zm; say "\n"
          end (* test *)

    val N = 21

    fun loop_np i np = if i > N
          then ()
          else (test np; loop_np (i+1) (np*2))

    fun doit () = loop_np 1 16

    fun testit () = loop_np 1 16

  end;
end

(* all.sml -- all sources for nbody *)
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

    val solarMass = 4.0 * Math.pi * Math.pi
    val daysPerYear = 365.24

    fun sqr (x : real) = x * x

    type planet = {
        x : real, y : real, z : real,
        vx : real, vy : real, vz : real,
        mass : real
      }

    fun mkPlanet (x, y, z, vx, vy, vz, m) : planet = {
            x=x, y=y, z=z, vx=vx, vy=vy, vz=vz, mass=m
          }

    fun offsetMomentum bodies = let
          val sun::r = bodies
          fun lp ([] : planet list, px, py, pz) = {
                  x = #x sun, y = #y sun, z = #z sun,
                  vx = #vx sun - (px / solarMass),
                  vy = #vy sun - (py / solarMass),
                  vz = #vz sun - (pz / solarMass),
                  mass = #mass sun
                }
            | lp ({vx, vy, vz, mass, ...}::r, px, py, pz) =
                lp (r, px + vx * mass, py + vy * mass, pz + vz * mass)
          in
            lp (bodies, 0.0, 0.0, 0.0) :: r
          end

    fun energy bodies = let
          fun lp ([] : planet list, e) = e
            | lp (b::br, e) = let
                val sq = sqr(#vx b) + sqr(#vy b) + sqr(#vz b)
                val e = e + 0.5 * #mass b * sq
                fun lp2 ([] : planet list, e) = e
                  | lp2 (b2::br2, e) = let
                      val dsq = sqr(#x b - #x b2) + sqr(#y b - #y b2) + sqr(#z b - #z b2)
                      val e = e - (#mass b * #mass b2) / Math.sqrt dsq
                      in
                        lp2 (br2, e)
                      end
                in
                  lp (br, lp2 (br, e))
                end
          in
            lp (bodies, 0.0)
          end

    fun advance (bodies : planet list, dt) = let
          fun lp1 ([], bs) = List.rev bs
            | lp1 (b::br, bs) = let
                (* compute change in velocity for each pair of (b, b2), where
                 * b2 is in the list br.
                 *)
                fun lp2 ([], b, bs2) = (b, List.rev bs2)
                  | lp2 (b2::br2, b, bs2) = let
                      (* signed distance from b2 to b *)
                      val dx = #x b - #x b2
                      val dy = #y b - #y b2
                      val dz = #z b - #z b2
                      val dsq = sqr dx + sqr dy + sqr dz
                      val mag = dt / (dsq * Math.sqrt dsq)
                      fun accelerate (b : planet, mass) = let
                            val m = mass * mag
                            in {
                              x = #x b, y = #y b, z = #z b,
                              vx = #vx b + dx * m,
                              vy = #vy b + dy * m,
                              vz = #vz b + dz * m,
                              mass = #mass b
                            } end
                      val b' = accelerate (b, ~(#mass b2))
                      val b2' = accelerate (b2, #mass b)
                      in
                        lp2 (br2, b', b2'::bs2)
                      end
                val (b', br') = lp2 (br, b, [])
                in
                  lp1 (br', b'::bs)
                end
          fun adjustPos (b : planet) = {
                  x = #x b + #vx b * dt,
                  y = #y b + #vy b * dt,
                  z = #z b + #vz b * dt,
                  vx = #vx b, vy = #vy b, vz = #vz b,
                  mass = #mass b
                }
          in
            List.map adjustPos (lp1 (bodies, []))
          end

    val bodies = [
            (* the sun *)
            mkPlanet (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, solarMass),
            (* jupiter *)
            mkPlanet (
              4.84143144246472090,
              ~1.16032004402742839,
              ~1.03622044471123109e~01,
              1.66007664274403694e~03 * daysPerYear,
              7.69901118419740425e~03 * daysPerYear,
              ~6.90460016972063023e~05 * daysPerYear,
              9.54791938424326609e~04 * solarMass),
            (* saturn *)
            mkPlanet (
              8.34336671824457987,
              4.12479856412430479,
              ~4.03523417114321381e~01,
              ~2.76742510726862411e~03 * daysPerYear,
              4.99852801234917238e~03 * daysPerYear,
              2.30417297573763929e~05 * daysPerYear,
              2.85885980666130812e~04 * solarMass),
            (* uranus *)
            mkPlanet (
              1.28943695621391310e01,
              ~1.51111514016986312e01,
              ~2.23307578892655734e~01,
              2.96460137564761618e~03 * daysPerYear,
              2.37847173959480950e~03 * daysPerYear,
              ~2.96589568540237556e~05 * daysPerYear,
              4.36624404335156298e~05 * solarMass),
            (* neptune *)
            mkPlanet (
              1.53796971148509165e01,
              ~2.59193146099879641e01,
              1.79258772950371181e~01,
              2.68067772490389322e~03 * daysPerYear,
              1.62824170038242295e~03 * daysPerYear,
              ~9.51592254519715870e~05 * daysPerYear,
              5.15138902046611451e~05 * solarMass)
          ]

    fun run (n, bodies) = let
          fun lp (0, bodies) = bodies
            | lp (i, bodies) = lp (i - 1, advance (bodies, 0.01))
          val bodies = lp (n, offsetMomentum bodies)
          in
            energy bodies
          end

    val name = "nbody"

    val results : string list = []

    fun doit () = ignore (run (50000000, offsetMomentum bodies))

    fun testit () = let
          fun pr e = Log.say [Real.fmt (StringCvt.FIX(SOME 9)) e, "\n"]
          val bodies = offsetMomentum bodies
          in
            pr (energy bodies);
            pr (run (1000, bodies))
          end

  end
end

(* all.sml -- all sources for aobench *)
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
(******************** ../BASIS/word8-array.sml ********************)
(* word8-array.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Word8Array : MONO_ARRAY =
  struct

    structure A = Unsafe.Word8Array
    structure V = Unsafe.Word8Vector

    (* fast add/subtract avoiding the overflow test *)
    infix -- ++
    fun x -- y = Word.toIntX(Word.fromInt x - Word.fromInt y)
    fun x ++ y = Word.toIntX(Word.fromInt x + Word.fromInt y)

    fun uLessThan (x, y) = Word.<(Word.fromInt x, Word.fromInt y)

  (* unchecked access operations *)
    val uupd = A.update
    val usub = A.sub
    val vuupd = V.update
    val vusub = V.sub
    val vlength = Word8Vector.length

    type array = A.array
    type elem = Word8.word
    type vector = Word8Vector.vector

    val maxLen = Word8Array.maxLen

    val vector0 : vector = V.create 0

    fun array (0, _) = Word8Array.array(0, 0w0)
      | array (len, v) = if (uLessThan(maxLen, len))
	    then raise General.Size
	    else let
	      val arr = A.create len
	      fun init i = if (i < len)
		    then (uupd(arr, i, v); init(i+1))
		    else ()
	      in
		init 0; arr
	      end

    fun tabulate (0, _) = Word8Array.array(0, 0w0)
      | tabulate (len, f) = if (uLessThan(maxLen, len))
	    then raise General.Size
	    else let
	      val arr = A.create len
	      fun init i = if (i < len)
		    then (uupd(arr, i, f i); init(i+1))
		    else ()
	      in
		init 0; arr
	      end

    fun fromList [] = Word8Array.array(0, 0w0)
      | fromList l = let
	  fun length ([], n) = n
	    | length (_::r, n) = length (r, n+1)
	  val len = length (l, 0)
	  val _ = if (maxLen < len) then raise General.Size else ()
	  val arr = A.create len
	  fun init ([], _) = ()
	    | init (c::r, i) = (uupd(arr, i, c); init(r, i+1))
	  in
	    init (l, 0); arr
	  end

    val length = Word8Array.length
    val sub    = Word8Array.sub
    val update = Word8Array.update

    fun vector a = (case length a
         of 0 => vector0
	  | len => let
              val v : Word8Vector.vector = V.create len
              fun fill i = if i >= len
                    then ()
                    else (vuupd (v, i, usub (a, i)); fill (i ++ 1))
              in
                fill 0; v
              end
        (* end cae *))

    fun copy { src, dst, di } = let
	val sl = length src
	val de = sl + di
	fun copyDn (s, d) =
	    if s < 0 then () else (uupd (dst, d, usub (src, s));
				   copyDn (s -- 1, d -- 1))
        in
          if di < 0 orelse de > length dst then raise Subscript
          else copyDn (sl -- 1, de -- 1)
        end

    fun copyVec { src, dst, di } = let
	val sl = vlength src
	val de = sl + di
	fun copyDn (s, d) =
	    if s < 0 then () else (uupd (dst, d, vusub (src, s));
				   copyDn (s -- 1, d -- 1))
        in
          if di < 0 orelse de > length dst then raise Subscript
          else copyDn (sl -- 1, de -- 1)
        end

    fun appi f arr = let
	val len = length arr
	fun app i =
	    if i >= len then () else (f (i, usub (arr, i)); app (i ++ 1))
        in
          app 0
        end

    fun app f arr = let
	val len = length arr
	fun app i =
	    if i >= len then () else (f (usub (arr, i)); app (i ++ 1))
        in
          app 0
        end

    fun modifyi f arr = let
	val len = length arr
	fun mdf i =
	    if i >= len then ()
	    else (uupd (arr, i, f (i, usub (arr, i))); mdf (i ++ 1))
        in
          mdf 0
        end

    fun modify f arr = let
	val len = length arr
	fun mdf i =
	    if i >= len then ()
	    else (uupd (arr, i, f (usub (arr, i))); mdf (i ++ 1))
        in
          mdf 0
        end

    fun foldli f init arr = let
	val len = length arr
	fun fold (i, a) =
	    if i >= len then a else fold (i ++ 1, f (i, usub (arr, i), a))
        in
          fold (0, init)
        end

    fun foldl f init arr = let
	val len = length arr
	fun fold (i, a) =
	    if i >= len then a else fold (i ++ 1, f (usub (arr, i), a))
        in
          fold (0, init)
        end

    fun foldri f init arr = let
	fun fold (i, a) =
	    if i < 0 then a else fold (i -- 1, f (i, usub (arr, i), a))
        in
          fold (length arr -- 1, init)
        end

    fun foldr f init arr = let
	fun fold (i, a) =
	    if i < 0 then a else fold (i -- 1, f (usub (arr, i), a))
        in
          fold (length arr -- 1, init)
        end

    fun findi p arr = let
	val len = length arr
	fun fnd i =
	    if i >= len then NONE
	    else let val x = usub (arr, i)
		 in
                   if p (i, x) then SOME (i, x) else fnd (i ++ 1)
		 end
        in
          fnd 0
        end

    fun find p arr = let
	val len = length arr
	fun fnd i =
	    if i >= len then NONE
	    else let val x = usub (arr, i)
		 in
		     if p x then SOME x else fnd (i ++ 1)
		 end
        in
          fnd 0
        end

    fun exists p arr = let
	val len = length arr
	fun ex i = i < len andalso (p (usub (arr, i)) orelse ex (i ++ 1))
        in
          ex 0
        end

    fun all p arr = let
	val len = length arr
	fun al i = i >= len orelse (p (usub (arr, i)) andalso al (i ++ 1))
        in
          al 0
        end

    fun collate c (a1, a2) = let
	val l1 = length a1
	val l2 = length a2
	val l12 = Int.min (l1, l2)
	fun coll i =
	    if i >= l12 then Int.compare (l1, l2)
	    else case c (usub (a1, i), usub (a2, i)) of
		     EQUAL => coll (i ++ 1)
		   | unequal => unequal
        in
          coll 0
        end

  (* added for Basis Library proposal 2015-003 *)
    fun toList arr = foldr op :: [] arr

    fun fromVector v = let
	  val n = vlength v
	  in
	    if (n = 0)
	      then Word8Array.array(0, 0w0)
	      else let
		val arr = A.create n
		fun fill i = if (i < n)
		      then (uupd(arr, i, vusub(v, i)); fill(i ++ 1))
		      else arr
		in
		  fill 0
		end
	  end

    val toVector = vector

  end (* structure Word8Array *)
(******************** ../common/rand48.sml ********************)
(* rand48.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * An SML implementation of the rand48 functions from the C standard
 * library.
 *
 * As described in the Open Group Specification, these routines work
 * by generating a sequence of 48-bit integer values, X{i}, according
 * to the linear congruential formula:
 *
 *      X{i+1} = (a * X{i} + c) mod m  for i >= 0
 *
 * where
 *
 *      m = 2^48
 *      a = 0x5DEECE66D = 25214903917
 *      c = 0xB = 11
 *)

signature RAND48 =
  sig

    (* `seed s` initializes the internal buffer with the 48 low-order
     * bits from `s`.  It returns the previous contents of the buffer.
     *)
    val seed : Word64.word -> Word64.word

    (* `srand w` sets the internal buffer to `(w << 16) | 0x330E` *)
    val srand : word -> unit

    (* returns a random real number in the range 0..1 *)
    val drand : unit -> real

    (* returns a random unsigned 31-bit number (i.e., in the range 0..2^31-1) *)
    val lrand : unit -> word

    (* returns a random signed 32-bit number (i.e., in the range -2^31..2^31-1) *)
    val mrand : unit -> int

  end

structure Rand48 : RAND48 =
  struct

    structure W64 = Word64

    type t = {
        x : W64.word ref,
        a : W64.word,
        c : W64.word
      }

    (* mask low 48 bits *)
    val mask48 = W64.<<(0w1, 0w48) - 0w1
    (* mask low 32 bits *)
    val mask32 = W64.<<(0w1, 0w32) - 0w1
    (* mask low 31 bits *)
    val mask31 = W64.<<(0w1, 0w31) - 0w1

    (* IEEE exponent bias *)
    val ieeeExpBias = Word64.<<(0wx3ff, 0w52)

    val buffer : t = { x = ref 0wx1234abcd330e, a = 0wx5deece66d, c = 0wxb }

    fun seed w = let
          val old = !(#x buffer)
          in
            #x buffer := W64.andb(w, mask48);
            old
          end

    fun srand w = let
          val w = W64.fromLarge(Word.toLarge w)
          in
            ignore (seed (W64.orb (W64.<< (w, 0w16), 0wx330e)))
          end

    fun randStep ({x, a, c} : t) = let
          val next = W64.andb(!x * a + c, mask48)
          in
            x := next; next
          end

    (* convert a 48-bit value to a IEEE double in the range [0..1) *)
    fun mkReal w = let
          val r = Unsafe.Real64.castFromWord (W64.orb(ieeeExpBias, W64.<<(w, 0w4)))
          in
            r - 1.0
          end

    fun drand () = mkReal (randStep buffer)

    fun lrand () = Word.fromLarge (W64.toLarge(W64.andb(randStep buffer, mask31)))

    (* returns a random signed 32-bit number (i.e., in the range -2^31..2^31-1) *)
    fun mrand () = let
          val w = W64.andb(randStep buffer, mask32)
          in
            W64.toIntX(W64.~>>(W64.<<(w, 0w32), 0w32))
          end

  end
(******************** aobench.sml ********************)
(* aobench.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure AOBench (*: sig

    val render : {ht : int, wid : int, nSubsamples : int} -> Word8Array.array

  end*) = struct

    structure W8A = Word8Array
    structure R64A = Real64Array

    val drand48 = Rand48.drand

  (** Scene parameters **)
    val naoSamples = 8

  (** for-loop combinators *)
    fun upto n f = let
          fun lp i = if (i < n) then (f i; lp(i+1)) else ()
          in
            lp 0
          end
    fun foldUpto n init f = let
          fun lp (i, acc) = if (i < n) then lp(i+1, f(i, acc)) else acc
          in
            lp (0, init)
          end

  (** Types **)
    type vec = {x : real, y : real, z : real}

    type isect = {t : real, p : vec, n : vec, hit : bool}

    type sphere = {center : vec, radius : real}

    type plane = {p : vec, n : vec}

    type ray = {org : vec, dir : vec}

  (** vector operations **)
    val vzero = {x = 0.0, y = 0.0, z = 0.0}
    fun vadd (v0 : vec, v1 : vec) = {
            x = #x v0 + #x v1, y = #y v0 + #y v1, z = #z v0 + #z v1
          }
    fun vsub (v0 : vec, v1 : vec) = {
            x = #x v0 - #x v1, y = #y v0 - #y v1, z = #z v0 - #z v1
          }
    fun vdot (v0 : vec, v1 : vec) = #x v0 * #x v1 + #y v0 * #y v1 + #z v0 * #z v1
    fun vcross (v0 : vec, v1 : vec) = {
            x = #y v0 * #z v1 - #z v0 * #y v1,
            y = #z v0 * #x v1 - #x v0 * #z v1,
            z = #x v0 * #y v1 - #y v0 * #x v1
          }
    fun vnormalize v = let
          val len = Math.sqrt (vdot (v, v))
          in
            if Real.abs len > 1.0e~17
              then {x = #x v / len, y = #y v / len, z = #z v / len}
              else v
          end

  (** ray operations **)
    fun pointAt (ray : ray, t : real) = {
            x = #x(#org ray) + t * #x(#dir ray),
            y = #y(#org ray) + t * #y(#dir ray),
            z = #z(#org ray) + t * #z(#dir ray)
          }

  (** intersection tests **)

    val miss : isect = {t = 1.0e17, p = vzero, n = vzero, hit = false}

    fun hit (t, p, n) : isect = {t = t, p = p, n = n, hit = true}

    fun raySphereIntersect (isect : isect, ray : ray, sphere : sphere) = let
          val rs = vsub(#org ray, #center sphere)
          val b = vdot(rs, #dir ray)
          val c = vdot(rs, rs) - #radius sphere * #radius sphere
          val d = b * b - c
          in
            if (d > 0.0)
              then let
                val t = ~b - Math.sqrt d
                in
                  if (t > 0.0) andalso (t < #t isect)
                    then let
                      val pt = pointAt (ray, t)
                      in
                        hit (t, pt, vnormalize(vsub(pt, #center sphere)))
                      end
                    else isect
                end
              else isect
          end

    fun rayPlaneIntersect (isect : isect, ray : ray, plane : plane) = let
          val d = ~(vdot(#p plane, #n plane))
          val v = vdot(#dir ray, #n plane)
          in
            if Real.abs v < 1.0e~17
              then isect
              else let
                val t = ~(vdot(#org ray, #n plane) + d) / v
                in
                  if (t > 0.0) andalso (t < #t isect)
                    then hit (t, pointAt(ray, t), #n plane)
                    else isect
                end
          end

    (* create an orthonormal basis `(vX, vY, n)` for a unit normal `n` *)
    fun orthoBasis (n : vec) = let
          val vZ = n
          val vY = if (#x n < 0.6) andalso (#x n > ~0.6)
                  then {x = 1.0, y = 0.0, z = 0.0}
                else if (#y n < 0.6) andalso (#y n > ~0.6)
                  then {x = 0.0, y = 1.0, z = 0.0}
                else if (#z n < 0.6) andalso (#z n > ~0.6)
                  then {x = 0.0, y = 0.0, z = 1.0}
                  else {x = 1.0, y = 0.0, z = 0.0}
          val vX = vnormalize (vcross (vY, vZ))
          val vY = vnormalize (vcross (vZ, vX))
          in
            (vX, vY, vZ)
          end

  (** The scene **)
    val sphere1 : sphere = { center = {x = ~2.0, y = 0.0, z = ~3.5 }, radius = 0.5 }
    val sphere2 : sphere = { center = {x = ~0.5, y = 0.0, z = ~3.0 }, radius = 0.5 }
    val sphere3 : sphere = { center = {x = 1.0, y = 0.0, z = ~2.2 }, radius = 0.5 }
    val plane : plane = {
            p = {x = 0.0, y = ~0.5, z = 0.0 },
            n = {x = 0.0, y = 1.0, z = 0.0 }
          }

  (** Rendering **)
    fun ambientOcclusion (isect : isect) = let
          val nTheta = naoSamples
          val nPhi = naoSamples
          val (vX, vY, vZ) = orthoBasis (#n isect)
          val eps = 0.0001
          val p = {
                  x = #x (#p isect) + eps * #x (#n isect),
                  y = #y (#p isect) + eps * #y (#n isect),
                  z = #z (#p isect) + eps * #z (#n isect)
                }
          val occlusion =
                foldUpto nTheta 0.0 (fn (j, occlusion) =>
                  foldUpto nPhi occlusion (fn (i, occlusion) => let
                    val theta = Math.sqrt(drand48())
                    val phi = 2.0 * Math.pi * drand48()
                    val x = Math.cos(phi) * theta
                    val y = Math.sin(phi) * theta
                    val z = Math.sqrt(1.0 - theta * theta)
                    val rx = x * #x vX + y * #x vY + z * #x vZ
                    val ry = x * #y vX + y * #y vY + z * #y vZ
                    val rz = x * #z vX + y * #z vY + z * #z vZ
                    val ray = {org = p, dir = {x = rx, y = ry, z = rz} }
                    val occIsect = raySphereIntersect(miss, ray, sphere1)
                    val occIsect = raySphereIntersect(occIsect, ray, sphere2)
                    val occIsect = raySphereIntersect(occIsect, ray, sphere3)
                    val occIsect = rayPlaneIntersect (occIsect, ray, plane)
                    in
                      if #hit occIsect then occlusion + 1.0 else occlusion
                    end))
          val nS = real(nTheta * nPhi)
          val occlusion = (nS - occlusion) / nS
          in
            { x = occlusion, y = occlusion, z = occlusion }
          end

    fun clamp f = let
          val i = Real.trunc(255.0 * f)
          in
            if (i < 0) then 0w0
            else if (i > 255) then 0w255
            else Word8.fromInt i
          end

    fun render {ht, wid, nSubsamples} = let
          val img = W8A.array(3 * ht * wid, 0w0)
          val fImg = R64A.array(3 * ht * wid, 0.0)
          fun index (row, col) = 3 * (row * wid + col)
          val nSubsamples2 = real(nSubsamples * nSubsamples)
          in
            upto ht (fn row =>
              upto wid (fn col => let
                val idx = index (row, col)
                val x = real col
                val y = real row
                in
                  upto nSubsamples (fn v =>
                    upto nSubsamples (fn u => let
                      val halfWid = real wid / 2.0
                      val px = (x + (real u / real nSubsamples) - halfWid) / halfWid
                      val halfHt = real ht / 2.0
                      val py = ~(y + (real v / real nSubsamples) - halfHt) / halfHt
                      val ray = { org = vzero, dir = vnormalize {x = px, y = py, z = ~1.0} }
                      val isect = raySphereIntersect(miss, ray, sphere1)
                      val isect = raySphereIntersect(isect, ray, sphere2)
                      val isect = raySphereIntersect(isect, ray, sphere3)
                      val isect = rayPlaneIntersect (isect, ray, plane)
                      in
                        if #hit isect
                          then let
                            val col = ambientOcclusion isect
                            in
                              R64A.update(fImg, idx+0, R64A.sub(fImg, idx+0) + #x col);
                              R64A.update(fImg, idx+1, R64A.sub(fImg, idx+1) + #y col);
                              R64A.update(fImg, idx+2, R64A.sub(fImg, idx+2) + #z col)
                            end
                          else ()
                      end));
                  R64A.update(fImg, idx+0, R64A.sub(fImg, idx+0) / nSubsamples2);
                  R64A.update(fImg, idx+1, R64A.sub(fImg, idx+1) / nSubsamples2);
                  R64A.update(fImg, idx+2, R64A.sub(fImg, idx+2) / nSubsamples2);
                  W8A.update(img, idx+0, clamp(R64A.sub(fImg, idx+0)));
                  W8A.update(img, idx+1, clamp(R64A.sub(fImg, idx+1)));
                  W8A.update(img, idx+2, clamp(R64A.sub(fImg, idx+2)))
                end));
            img
          end (* render *)

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

    val name = "aobench"

    fun savePPM (fname, wid, ht, img) = let
          val outS = BinIO.openOut fname
          in
            BinIO.output (outS, Byte.stringToBytes (concat [
                "P6\n", Int.toString wid, " ", Int.toString ht, "\n255\n"
              ]));
            Word8Array.app (fn b => BinIO.output1 (outS, b)) img;
            BinIO.closeOut outS
          end

    fun doit () = ignore (AOBench.render{wid = 512, ht = 512, nSubsamples = 3})

    fun testit () = (
          savePPM (
            "out.ppm", 256, 256,
            AOBench.render{wid = 256, ht = 256, nSubsamples = 2});
          Log.print "OK\n")

    val results = ["out.ppm"]

  end
end

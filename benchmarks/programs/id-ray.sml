(* all.sml -- all sources for id-ray *)
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
(******************** ../BASIS/mono-array.sig ********************)
(* mono-array.sig
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Generic interface for monomorphic array structures.
 *)

signature MONO_ARRAY =
  sig

    eqtype array
    type elem
    type vector

    val maxLen : int

  (* array creation functions *)
    val array    : int * elem -> array
    val fromList : elem list -> array
    val tabulate : int * (int -> elem) -> array

    val length   : array -> int
    val sub      : array * int -> elem
    val update   : array * int * elem -> unit

    val vector   : array -> vector
    val copy     : { src : array, dst : array, di : int } -> unit
    val copyVec  : { src : vector, dst : array, di : int } -> unit


    val appi   : (int * elem -> unit) -> array -> unit
    val app    : (elem -> unit) -> array -> unit
    val modifyi: (int * elem -> elem) -> array -> unit
    val modify : (elem -> elem) -> array -> unit

    val foldli : (int * elem * 'a -> 'a) -> 'a -> array -> 'a
    val foldri : (int * elem * 'a -> 'a) -> 'a -> array -> 'a
    val foldl  : (elem * 'a -> 'a) -> 'a -> array -> 'a
    val foldr  : (elem * 'a -> 'a) -> 'a -> array -> 'a

    val findi   : (int * elem -> bool) -> array -> (int * elem) option
    val find    : (elem -> bool) -> array -> elem option
    val exists  : (elem -> bool) -> array -> bool
    val all     : (elem -> bool) -> array -> bool
    val collate : (elem * elem -> order) -> array * array -> order

    (* Basis Library proposal 2015-003 *)
    val toList     : array -> elem list
    val fromVector : vector -> array
    val toVector   : array -> vector
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
(******************** image.sml ********************)
(* image.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Image : sig

    type t

    val new : int * int -> t
    val update : t * int * int * real * real * real -> unit
    val output : t * string -> unit

  end = struct

    structure BinIO = Log.BinIO

    datatype t = IMG of {
        wid : int,
        ht : int,
        data : Word8Array.array
      }

    fun clamp d = if d < 0.0 then 0.0 else if d > 1.0 then 1.0 else d

    fun new (w, h) = IMG{
            wid = w, ht = h,
            data = Word8Array.array(3 * w * h, 0w0)
          }

    fun update (IMG{wid, ht, data}, row, col, r, g, b) = let
  	  fun cvt x = Word8.fromInt(Real.round((clamp x) * 255.0))
          val idx = wid * row + col
          in
            Word8Array.update(data, idx+0, cvt r);
            Word8Array.update(data, idx+1, cvt g);
            Word8Array.update(data, idx+2, cvt b)
          end

    fun output (IMG{wid, ht, data}, outFile) = let
          val outS = BinIO.openOut outFile
	  fun pr s = BinIO.output(outS, Byte.stringToBytes s)
          in
	    pr "P6\n";
	    pr (concat[Int.toString wid, " ", Int.toString ht, "\n"]);
	    pr "255\n";
(* NOTE: the SML Basis Library should provide an output function for arrays! *)
            Word8Array.app (fn b => BinIO.output1(outS, b)) data;
            BinIO.closeOut outS
          end

  end
(******************** ray.sml ********************)
(* ray.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Ray : sig

    (* generate the test image to the given file *)
    val ray : string * int -> unit

  end = struct

    (* definitions to match the Manticore basis *)
    type double = real
    fun fail msg = raise Fail msg

    (* images *)
    type image = Image.t
    val newImage = Image.new
    val updateImage3d = Image.update
    val outputImage = Image.output

    val sqrt = Math.sqrt;
    fun expt a = let fun expt' b = Math.pow(a, b) in expt' end;
    val pi : double = 3.14159265359;
    (*
     * generally handy stuff
     *)
    val EPSILON : double = 1.0e~6;
    val INFINITY : double = 1.0e20;
    fun map f = let
	  fun mapf l = (case l of nil => nil | x::xs => f x :: mapf xs)
	  in
	    mapf
	  end;
    fun fold f = let
	  fun foldf s0 = let
		fun fold' l = (case l of nil => s0 | x::xs => f (x, foldf s0 xs))
		in
		  fold'
		end
	  in
	    foldf
	  end;
    fun hd l = (case l
	   of nil => fail("expecting a head")
	    | x::xs => x);
    fun tl l = (case l
	   of nil => fail("expecting a tail")
	    | x::xs => xs);
    (*
     * convenient vector operations
     *)
    type vec = (double * double * double);
    fun vecadd ((x1,y1,z1) : vec) = let fun add (x2,y2,z2) = (x1+x2, y1+y2, z1+z2) in add end;
    fun vecsum (x : vec list) = let
	  fun f (a, b) = vecadd a b
	  in
	    fold f (0.0,0.0,0.0) x
	  end;
    fun vecsub ((x1,y1,z1) : vec) = let fun sub (x2,y2,z2) = (x1-x2, y1-y2, z1-z2) in sub end;
    fun vecmult ((x1,y1,z1) : vec) = let fun mul (x2,y2,z2) = (x1*x2, y1*y2, z1*z2) in mul end;
    fun vecnorm ((x,y,z) : vec) = let
	  val len = sqrt (x*x + y*y + z*z)
	  in ((x/len, y/len, z/len), len) end;
    fun vecscale ((x,y,z) : vec) = let fun scale a = (a*x, a*y, a*z) in scale end;
    fun vecdot ((x1,y1,z1) : vec) = let fun dot (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2 in dot end;
    fun veccross ((x1,y1,z1) : vec) = let fun cross (x2,y2,z2) = (y1*z2-y2*z1, z1*x2-z2*x1, x1*y2-x2*y1) in cross end;
    (* Note the following code is broken for negative vectors, but it was in the original
     * version.
     *)
    fun zerovector ((x,y,z) : vec) =
	  (x < EPSILON andalso y < EPSILON andalso z < EPSILON);

    (*
     * type declarations
     *)
    datatype Light
      = Directional of (vec * vec)		(* direction, color *)
      | Point of (vec * vec)		(* position, color *)
      ;
    fun lightcolor l = (case l
	   of (Directional(_, c)) => c
	    | (Point(_, c)) => c
	  (* end case *));
    datatype Surfspec
      = Ambient of vec	(* all but specpow default to zero *)
      | Diffuse of vec
      | Specular of vec
      | Specpow of double	(* default 8. *)
      | Reflect of double
      | Transmit of double
      | Refract of double	(* default 1, like air == no refraction *)
      | Body of vec		(* body color, default 1.,1.,1. *)
      ;
    fun ambientsurf surf = (case surf
	   of nil => (0.0, 0.0, 0.0)
	    | (Ambient v :: ss) => v
	    | (_ :: ss) => ambientsurf ss
	  (* end case *));
    fun diffusesurf surf = (case surf
	   of nil => (0.0, 0.0, 0.0)
	    | (Diffuse v :: ss) => v
	    | (_ :: ss) => diffusesurf ss
	  (* end case *));
    fun specularsurf surf = (case surf
	   of nil => (0.0, 0.0, 0.0)
	    | (Specular v :: ss) => v
	    | (_ :: ss) => (specularsurf ss)
	  (* end case *));
    fun specpowsurf surf = (case surf
	   of nil => 8.0
	    | (Specpow r :: ss) => r
	    | (_ :: ss) => specpowsurf ss
	  (* end case *));
    fun reflectsurf surf = (case surf
	   of nil => 0.0
	    | (Reflect r :: ss) => r
	    | (_ :: ss) => reflectsurf ss
	  (* end case *));
    fun transmitsurf surf = (case surf
	   of nil => 0.0
	    | (Transmit r :: ss) => r
	    | (_ :: ss) => transmitsurf ss
	  (* end case *));
    fun refractsurf surf = (case surf
	   of nil => 1.0
	    | (Refract r :: ss) => r
	    | (_ :: ss) => refractsurf ss
	  (* end case *));
    fun bodysurf surf = (case surf
	   of nil => (1.0,1.0,1.0)
	    | (Body v :: ss) => v
	    | (_ :: ss) => bodysurf ss
	  (* end case *));

    datatype Sphere = Sphere of vec * double * Surfspec list; (* pos, radius, surface type *)
    fun spheresurf (Sphere(pos, rad, surf)) = surf;

    (*
    % camera static:
    %   lookfrom = 0 -10 0   <--- Camera.pos
    %   lookat = 0 0 0
    %   vup = 0 0 1
    %   fov = 45
    % yields
    %   dir = norm(lookat - lookfrom) = 0 1 0
    %   lookdist = length(lookat-lookfrom) = 10
    *)
    (*
     * test conditions
     *)
    (*val lookfrom = (0.0, (-10.0), 0.0);*)
    val lookat = (0.0, 0.0, 0.0);
    val vup = (0.0, 0.0, 1.0);
    val fov = 45.0;
    (*val background = (0.1, 0.1, 0.2);*)

    val redsurf = (Ambient (0.1,0.0,0.0)) ::(Diffuse (0.3,0.0,0.0)) ::
	       (Specular (0.8,0.4,0.4)) :: (Transmit 0.7) :: nil;
    val greensurf = (Ambient (0.0,0.1,0.0)) :: (Diffuse (0.0,0.3,0.0)) ::
		 (Specular (0.4,0.8,0.4)) :: nil;
    val bluesurf = (Ambient (0.0,0.0,0.1)) :: (Diffuse (0.0,0.0,0.3)) ::
		(Specular (0.4,0.4,0.8)) :: nil;
    (*
    val testspheres = ((Sphere ((0.0,0.0,0.0), 2.0, redsurf))::
		   (Sphere (((~2.1),(~2.0),(~2.2)), 0.5, bluesurf))::
		   (Sphere (((~2.8),3.5,(~1.8)), 1.7, greensurf)::nil));
    val testlights = (Directional ((1.0,(~1.0),1.0), (1.0,1.0,1.0)))::
		 (Point (((~3.0),(~3.0),(~3.0)), (1.0,1.0,1.0))::nil);
    *)
    (*%%%%%
    %% trivial transmission test
    % testspheres = ((Sphere ((-1.5),0.,0.) 3. redsurf)::
    % 	       (Sphere (1.5, 7.5, 0.) 4. greensurf)::nil);
    %%%%%%%
    %% reflection test
    % mirrorsurf = ((Ambient (.04,.04,.04))::(Diffuse (.05,.05,.05))::
    % 	      (Specular (.8,.8,.8))::(Specpow 60.)::(Reflect 1.)::nil);
    % testspheres = ((Sphere ((-1.5),0.,0.) 2. mirrorsurf)::
    % 	       (Sphere (1.,(-2.),(-.5)) 1. greensurf)::nil);
    *)
    (*%%%%%%
    %% standard balls
    *)
    val s2 = (Ambient (0.035,0.0325,0.025)) :: (Diffuse(0.5,0.45,0.35)) ::
	   (Specular(0.8,0.8,0.8)) :: (Specpow 3.0) :: (Reflect 0.5) :: nil;
    val s3 = (Ambient (0.1,0.0,0.0)) :: (Diffuse (0.3,0.0,0.0)) ::
	       (Specular (0.8,0.4,0.4)) :: (Transmit 0.7) :: nil;


    fun readFromFile () =
	let
	    val f = TextIO.openIn "DATA/spheres.txt"
	    fun rd d = Option.valOf (Real.fromString d)
	    fun lp acc =
		(case TextIO.inputLine f
		  of NONE => List.rev acc
		   | SOME line =>
		     let
			 val x::y::z::r::nil = List.map rd (String.tokens (fn c => c = #" ") line)
		     in
			 lp(Sphere((x,y,z),r, s2) :: acc)
		     end)
	in
	    lp nil
	end

    val testspheres =
	 Sphere((0.0,0.0,0.0), 0.5, s3) ::
	 Sphere((0.272166,0.272166,0.544331), 0.166667, s2) ::
	 Sphere((0.643951,0.172546,0.0), 0.166667, s2) ::
	 Sphere((0.172546,0.643951,0.0), 0.166667, s2) ::
	 Sphere(((~0.371785),0.0996195,0.544331), 0.166667, s2) ::
	 Sphere(((~0.471405),0.471405,0.0), 0.166667, s2) ::
	 Sphere(((~0.643951),(~0.172546),0.0), 0.166667, s2) ::
	 Sphere((0.0996195,(~0.371785),0.544331), 0.166667, s2) ::
	 Sphere(((~0.172546),(~0.643951),0.0), 0.166667, s2) ::
	 Sphere((0.471405,(~0.471405),0.0), 0.166667, s2) :: nil;
    val testlights = Point((4.0,3.0,2.0), (0.288675,0.288675,0.288675)) ::
		  Point((1.0, ~4.0,4.0), (0.288675,0.288675,0.288675)) ::
		  Point((~3.0,1.0,5.0), (0.288675,0.288675,0.288675)) :: nil;

    val lookfrom = (2.1, 1.3, 1.7);
    val background = (0.078, 0.361, 0.753);
    val world = testspheres @ (readFromFile ())

    (*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% sphere specific items
    %
    % figure when a ray hits a sphere
    %
    %%% Assumes direction vector is normalized!
    *)
    fun sphereintersect (pos, dir, sp) = let
	val Sphere(center, rad, _) = sp
	val m = vecsub pos center;  (* x - center *)
	val m2 = vecdot m m;    (* (x-center).(x-center) *)
	val bm = vecdot m dir;  (* (x-center).dir *)
	val disc = bm * bm - m2 + rad * rad;  (* discriminant *)
	in
	  if (disc < 0.0) then (false, 0.0)  (* imaginary solns only *)
	  else let
	      val slo = ~bm - (sqrt disc);
	      val shi = ~bm + (sqrt disc);
	      in
	      if (slo < 0.0) then  (* pick smallest positive intersection *)
		  if (shi < 0.0) then (false, 0.0)
		  else (true, shi)
	      else (true, slo)
	      end
	end;

    (*
    % for shading, need normal at a point
    *)
    fun spherenormal (pos, sp) = let
	  val Sphere(spos, rad, _) = sp;
	  in
	    vecscale (vecsub pos spos) (1.0/rad)
	  end;

    (*
    % compute camera parameters
    *)
    fun dtor x = x * pi / 180.0;
    fun camparams (lookfrom, lookat, vup, fov, winsize) = let
	val initfirstray = vecsub lookat lookfrom;   (* pre-normalized! *)
	val (lookdir, dist) = vecnorm initfirstray;
	val (scrni, _) = vecnorm (veccross lookdir vup);
	val (scrnj, _) = vecnorm (veccross scrni lookdir);
	val xfov = fov;
	val yfov = fov;
	val xwinsize = (real winsize);  (* for now, square window *)
	val ywinsize = (real winsize);
	val magx = 2.0 * dist * (Math.tan (dtor (xfov / 2.0))) / xwinsize;
	val magy = 2.0 * dist * (Math.tan (dtor (yfov / 2.0))) / ywinsize;
	val scrnx = vecscale scrni magx;
	val scrny = vecscale scrnj magy;
	val firstray = (vecsub initfirstray
	      (vecadd
	       (vecscale scrnx (0.5 * xwinsize))
	       (vecscale scrny (0.5 * ywinsize))));
	in
	  (firstray, scrnx, scrny)
	end;

    (*
    % color the given pixel
    *)
    fun tracepixel (spheres, lights, x, y, firstray, scrnx, scrny) = let
      val pos = lookfrom;
      val (dir, _) = vecnorm (vecadd (vecadd firstray (vecscale scrnx (real x)))
			(vecscale scrny (real y)));
      val (hit, dist, sp) = trace (spheres, pos, dir);  (* pick first intersection *)
						    (* return color of the pixel x,y *)
      in
	if hit then
	  shade (lights, sp, pos, dir, dist, (1.0,1.0,1.0))
	else
	  background
      end

    (*
    % find first intersection point in set of all objects
    *)
    and trace (spheres, pos, dir) = let
	(* make a list of the distances to intersection for each hit object *)
	fun sphmap l = (case l
	       of nil => nil
		| (x::xs) => let
		  val (hit, where') = sphereintersect (pos, dir, x)
		  in
		    if hit then
		      (where', x) :: (sphmap xs)
		    else
		      (sphmap xs)
		  end)
	val dists = sphmap spheres;
	(* return a sphere and its distance *)
	in
	  case dists
	   of nil => (false, INFINITY, (hd spheres))  (* missed all *)
	    | first::rest => let
		fun min ((d1, s1), (d2, s2)) = if (d1 < d2) then (d1,s1) else (d2,s2)
		val (mindist, sp) = fold min first rest
		in
		  (true, mindist, sp)
		end
	end

    (*
    % complete shader, given set of lights, sphere which was hit, ray which hit
    %   that sphere, and at what distance, return a color
    % contrib answers "what's the most my result can add to the working pixel?"
    %   and will abort a reflected or transmitted ray if it gets too small
    *)
    (*
    def testpos = 0.0,(-10.0),0.0;
    def testdir = (-0.23446755301152356),0.9434245773614214,(-0.23446755301152356);
    def testhitpos = (-1.9015720859580605), (-2.3486648004165893), (-1.9015720859580605);

    def testshade _ =
      {(hit?, dist, sp) = trace world testpos testdir;  % pick first intersection
       in
    %     shade testlights sp testpos testdir dist (1.,1.,1.)
    %     (hit?, dist, sp)
	 spherenormal testhitpos sp
      };
    *)

    and shade (lights, sp, lookpos, dir, dist, contrib) = let
	val hitpos = vecadd lookpos (vecscale dir dist);
	val ambientlight = (1.0, 1.0, 1.0);  (* full contribution as default *)
	val surf = spheresurf sp;
	val amb = vecmult ambientlight (ambientsurf surf);
	(*  reflected_ray_dir = incoming_dir - (2 cos theta) norm; *)
	val norm = spherenormal (hitpos, sp);
	val refl = vecadd dir (vecscale norm ((~2.0)*(vecdot dir norm)));
	(*  diff is diffuse and specular contribution *)
	fun lightray' l = lightray (l, hitpos, norm, refl, surf)
	val diff = vecsum (map lightray' lights);
	val transmitted = transmitsurf surf;
	val simple = vecadd amb diff;
	(* calculate transmitted ray; it adds onto "simple" *)
	val trintensity = vecscale (bodysurf surf) transmitted;
	val (tir, trcol) = if (transmitted < EPSILON) then (false, simple)
			else let
			  val index = refractsurf surf;
			  in
			    transmitray (lights, simple, hitpos, dir, index, trintensity,
			      contrib, norm)
			  end
	(*  reflected ray; in case of TIR, add transmitted component *)
	val reflsurf = vecscale (specularsurf surf) (reflectsurf surf);
	val reflectiv = if tir then (vecadd trintensity reflsurf) else reflsurf;
	val rcol = if (zerovector reflectiv) then
		 trcol
	       else
		 reflectray (hitpos, refl, lights, reflectiv, contrib, trcol);
       in
	 rcol
       end

    (*
    % Transmit a ray through an object
    *)
    and transmitray (lights, color, pos, dir, index, intens, contrib, norm) = let
	val newcontrib = vecmult intens contrib;
	in
	  if (zerovector newcontrib) then (false, color)  (* cutoff *)
	  else let
	    val (tir, newdir) = refractray (index, dir, norm);
	    in
	      if tir then (true, color)
	      else let
		  val nearpos = vecadd pos (vecscale newdir EPSILON);
		  val (hit, dist, sp) = trace (world, nearpos, newdir);
		  val newcol = if hit then
		      shade (lights, sp, nearpos, newdir, dist, newcontrib)
		      else background;
		  in (false, vecadd (vecmult newcol intens) color)
		  end
	    end
	end

    (*
     * Reflect a ray from an object
    *)
    and reflectray (pos, newdir, lights, intens, contrib, color) = let
	val newcontrib = vecmult intens contrib;
	in
	if (zerovector newcontrib) then color
	else let
	    val nearpos = vecadd pos (vecscale newdir EPSILON);
	    val (hit, dist, sp) = trace (world, nearpos, newdir);
	    val newcol = if (hit) then shade (lights, sp, nearpos, newdir, dist, newcontrib)
	    else background
	    in (vecadd color (vecmult newcol intens))
	end
    end

    (*
     * refract a ray through a surface (ala Foley, vanDamm, p. 757)
     *   outputs a new direction, and if total internal reflection occurred or not
     *)
    and refractray (newindex, olddir, innorm) = let
	val dotp = ~(vecdot olddir innorm);
	val (norm, k, nr) = if (dotp < 0.0)
	    then (vecscale innorm (~1.0), ~dotp, 1.0/newindex)
	    else (innorm, dotp, newindex);   (* trans. only with air *)
	val disc = 1.0 - nr*nr*(1.0-k*k);
	in if (disc < 0.0) then (true, (0.0,0.0,0.0)) (* total internal reflection *)
	else let
	    val t = nr * k - (sqrt disc);
	    in (false, vecadd (vecscale norm t) (vecscale olddir nr))
	end
    end

    (*
     * For a given light l, surface hit at pos, with norm and refl components
     * to incoming ray, figure out which side of the surface the light is on,
     * and if it's shadowed by another object in the world.  Return light's
     * contribution to the object's color
    *)
    and lightray (l, pos, norm, refl, surf) = let
	val (ldir, dist) = lightdirection (l, pos);
	val cosangle = vecdot ldir norm;  (* lightray is this far off normal *)
	val (inshadow, lcolor) = shadowed (pos, ldir, lightcolor l);
	in
	if (inshadow) then (0.0,0.0,0.0)
	else let
	    val diff = diffusesurf surf;
	    val spow = specpowsurf surf;  (* assumed trans is same as refl *)
	    in
	    if (cosangle <= 0.0) then let (* opposite side *)
		val bodycol = bodysurf surf;
		val cosalpha = ~(vecdot refl ldir);
		val diffcont = vecmult (vecscale diff (~cosangle)) lcolor;
		val speccont = if (cosalpha <= 0.0) then (0.0,0.0,0.0)
		    else vecmult (vecscale bodycol (expt cosalpha spow)) lcolor;
		in vecadd diffcont speccont
	    end else let
		val spec = specularsurf surf;
		val cosalpha = vecdot refl ldir;  (* this far off refl ray (for spec) *)
		val diffcont = vecmult (vecscale diff cosangle) lcolor;
		val speccont = if (cosalpha <= 0.0) then (0.0,0.0,0.0)
		    else vecmult (vecscale spec (expt cosalpha spow)) lcolor;
		in vecadd diffcont speccont
	    end
	end
    end

    and lightdirection (dir, pt) = (case dir
	   of (Directional(dir, col)) => let
		val (d,_) = vecnorm dir in (d, INFINITY) end
	    | (Point(pos, col)) => vecnorm (vecsub pos pt)
	  (* end case *))

    and shadowed (pos, dir, lcolor) = let (* need to offset just a bit *)
	val (hit, dist, sp) = trace (world, vecadd pos (vecscale dir EPSILON), dir);
	in
	  if (not hit) then (false, lcolor)
	  else (true, lcolor)  (* for now *)
	end;

    (*
    % "main" routine
    *)

    (* sequential version of the code *)
    fun ray (outFile, winsize) = let
	  val lights = testlights;
	  val (firstray, scrnx, scrny) = camparams (lookfrom, lookat, vup, fov, winsize);
	  val img = newImage (winsize, winsize)
	  fun f (i, j) = let
		val (r, g, b) = tracepixel (world, lights, i, j, firstray, scrnx, scrny)
		in
		  updateImage3d (img, i, j, r, g, b)
		end
	  fun lp i = if (i < winsize)
		then let
		  fun lp' j = if (j < winsize)
			then (f(i, j); lp'(j+1))
			else ()
		  in
		    lp' 0; lp(i+1)
		  end
		else ();
	  val _ = lp 0;
	  in
	    outputImage(img, "out.ppm")
	  end;

  end
in
(******************** main.sml ********************)
(* main.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "id-ray"

    val results = ["out.ppm"]

    fun testit () = (Ray.ray ("out.ppm", 256); Log.print "OK\n")
          handle _ => Log.print "FAIL\n"

    fun doit () = Ray.ray ("out.ppm", 1024)

  end
end

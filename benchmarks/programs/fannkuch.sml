(* all.sml -- all sources for fannkuch *)
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
(******************** ../BASIS/array.sig ********************)
(* array.sig
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ARRAY_2004 =
  sig
    type 'a array
    type 'a vector

    val maxLen   : int

    val array    : int * 'a -> 'a array
    val fromList : 'a list -> 'a array
    val tabulate : int * (int -> 'a) -> 'a array

    val length   : 'a array -> int
    val sub      : 'a array * int -> 'a
    val update   : 'a array * int * 'a -> unit

    val vector   : 'a array -> 'a vector

    val copy     : { src : 'a array, dst : 'a array, di : int } -> unit
    val copyVec  : { src : 'a vector, dst : 'a array, di : int } -> unit

    val appi    : (int * 'a -> unit) -> 'a array -> unit
    val app     : ('a -> unit) -> 'a array -> unit
    val modifyi : (int * 'a -> 'a) -> 'a array -> unit
    val modify  : ('a -> 'a) -> 'a array -> unit
    val foldli  : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldri  : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldl   : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldr   : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b

    val findi   : (int * 'a -> bool) -> 'a array -> (int * 'a) option
    val find    : ('a -> bool) -> 'a array -> 'a option
    val exists  : ('a -> bool) -> 'a array -> bool
    val all     : ('a -> bool) -> 'a array -> bool
    val collate : ('a * 'a -> order) -> 'a array * 'a array -> order
  end

(* includes Basis Library proposal 2015-003 *)
signature ARRAY_2015 =
  sig
    include ARRAY_2004

    val toList     : 'a array -> 'a list
    val fromVector : 'a vector -> 'a array
    val toVector   : 'a array -> 'a vector

  end

signature ARRAY = ARRAY_2015
(******************** ../BASIS/array.sml ********************)
(* array.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Array : ARRAY =
  struct

    structure U = Unsafe

    type 'a array = 'a Array.array
    type 'a vector = 'a Array.vector

    (* fast add/subtract avoiding the overflow test *)
    infix -- ++
    fun x -- y = Word.toIntX(Word.fromInt x - Word.fromInt y)
    fun x ++ y = Word.toIntX(Word.fromInt x + Word.fromInt y)

    fun ltu (i, n) = Word.<(Word.fromInt i, Word.fromInt n)

    val maxLen = Array.maxLen

    fun array (0, _) = U.Array.create0()
      | array (n, x) = if ltu(maxLen, n)
          then raise General.Size
          else U.Array.create(n, x)

    fun fromList [] = U.Array.create0()
      | fromList (l as (first::rest)) =
          let fun len(_::_::r, i) = len(r, i ++ 2)
                | len([x], i) = i ++ 1
                | len([], i) = i
              val n = len(l, 0)
              val a = array(n, first)
              fun fill (i, []) = a
                | fill (i, x::r) = (U.Array.update(a, i, x); fill(i ++ 1, r))
           in fill(1, rest)
          end

    fun tabulate (0, _) = U.Array.create0()
      | tabulate (n, f : int -> 'a) : 'a array =
          let val a = array(n, f 0)
              fun tab i =
                if (i < n) then (U.Array.update(a, i, f i);
				 tab(i ++ 1))
                else a
           in tab 1
          end


    val length : 'a array -> int = Array.length
    val sub : 'a array * int -> 'a = Array.sub
    val update : 'a array * int * 'a -> unit = Array.update

    val usub = U.Array.sub
    val uupd = U.Array.update
    val vusub = U.Vector.sub
    val vlength = Vector.length

    fun copy { src, dst, di } = let
	val sl = length src
	val de = sl + di
	fun copyDn (s,  d) =
	    if s < 0 then () else (uupd (dst, d, usub (src, s));
				   copyDn (s -- 1, d -- 1))
    in
	if di < 0 orelse de > length dst then raise Subscript
	else
	    copyDn (sl -- 1, de -- 1)
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
	    if i < len then (f (i, usub (arr, i)); app (i ++ 1))
	    else ()
    in
	app 0
    end

    fun app f arr = let
	val len = length arr
	fun app i =
	    if i < len then (f (usub (arr, i)); app (i ++ 1))
	    else ()
    in
	app 0
    end

    fun modifyi f arr = let
	val len = length arr
	fun mdf i =
	    if i < len then (uupd (arr, i, f (i, usub (arr, i))); mdf (i ++ 1))
	    else ()
    in
	mdf 0
    end

    fun modify f arr = let
	val len = length arr
	fun mdf i =
	    if i < len then (uupd (arr, i, f (usub (arr, i))); mdf (i ++ 1))
	    else ()
    in
	mdf 0
    end

    fun foldli f init arr = let
	val len = length arr
	fun fold (i, a) =
	    if i < len then fold (i ++ 1, f (i, usub (arr, i), a)) else a
    in
	fold (0, init)
    end

    fun foldl f init arr = let
	  val len = length arr
	  fun fold (i, a) =
	      if i < len then fold (i ++ 1, f (usub (arr, i), a)) else a
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

    (* FIXME: this is inefficient (going through intermediate list) *)
    fun vector arr = Vector.fromList (toList arr)

  (* added for Basis Library proposal 2015-003 *)
    fun fromVector vec = let
	  val n = vlength vec
	  in
	    if (n = 0)
	      then U.Array.create0()
	      else let
		val arr = array(n, U.Vector.sub(vec, 0))
		fun fill i = if (i < n)
		      then (
			U.Array.update(arr, i, vusub(vec, i));
			fill (i ++ 1))
		      else arr
		in
		  fill 1
		end
	  end

  (* added for Basis Library proposal 2015-003 *)
    val toVector = vector

end (* structure Array *)
in
(******************** main.sml ********************)
(* main.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    structure A = Array

    val name = "fannkuch"

    val results = []

    fun for (start, stop, f) = let
          fun lp i = if (i <= stop)
                then (f i; lp(i+1))
                else ()
          in
            lp start
          end

    fun countFlips perm = let
          (* count the number of flips before '0' is the first element *)
          fun loop c = let
                val k = A.sub (perm, 0)
                in
                  if k = 0 then c
                    else (
                      (* reverse the elements [0..k] *)
                      for (0, k div 2, fn i => let
                         val k_i = k - i
                         val perm_i = A.sub (perm, i)
                         in
                           A.update (perm, i, A.sub (perm, k_i));
                           A.update (perm, k_i, perm_i)
                         end);
                      loop (c + 1))
                end
          in
            loop 0
          end

    fun fannkuch n = let
          val perm = A.array (n, 0)
          val perm1 = A.tabulate (n, fn i => i)
          val count = A.array (n, 0)
          fun loop (0, maxFlips, checkSum, _) = (maxFlips, checkSum)
            | loop (r, maxFlips, checkSum, nPerm) = let
                val () = for (0, n-1, fn i => A.update(perm, i, A.sub(perm1, i)))
                val r = let
                      fun lp 1 = 1
                        | lp r = (A.update (count, r-1, r); lp(r-1))
                      in
                        lp r
                      end
                val nFlips = countFlips perm
                val maxFlips = Int.max(maxFlips, nFlips)
                val checkSum = if Word.andb(Word.fromInt nPerm, 0w1) = 0w0
                      then checkSum + nFlips
                      else checkSum - nFlips
                fun lp r = if (r = n) then 0
                      else let
                        val t = A.sub(perm1, 0)
                        in
                          for (0, r-1, fn i => A.update(perm1, i, A.sub(perm1, i+1)));
                          A.update(perm1, r, t);
                          A.update(count, r, A.sub(count, r) - 1);
                          if A.sub(count, r) > 0
                            then r
                            else lp (r + 1)
                        end
                in
                  loop (lp r, maxFlips, checkSum, nPerm+1)
                end
          val (maxFlips, checkSum) = loop (n, 0, 0, 0)
          in
            Log.say [
                Int.toString checkSum,
                "\nPfannkuchen(", Int.toString n, ") = ", Int.toString maxFlips, "\n"
              ]
          end

    fun testit () = fannkuch 7

    fun doit () = (fannkuch 11; fannkuch 11; fannkuch 11)

  end
end

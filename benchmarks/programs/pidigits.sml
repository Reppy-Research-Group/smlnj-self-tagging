(* all.sml -- all sources for pidigits *)
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
(******************** ../BASIS/mono-vector.sig ********************)
(* mono-vector.sig
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Generic interface for monomorphic vector structures.  Note that this
 * includes the various Basis Library proposals.
 *)

signature MONO_VECTOR =
  sig

    type vector
    type elem

    val maxLen : int

  (* vector creation functions *)
    val fromList : elem list -> vector
    val tabulate : int * (int -> elem) -> vector

    val length   : vector -> int
    val sub      : vector * int -> elem
    val concat   : vector list -> vector

    val update : vector * int * elem -> vector

    val appi   : (int * elem -> unit) -> vector -> unit
    val app    : (elem -> unit) -> vector -> unit
    val mapi   : (int * elem -> elem) -> vector -> vector
    val map    : (elem -> elem) -> vector -> vector
    val foldli : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldri : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldl  : (elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldr  : (elem * 'a -> 'a) -> 'a -> vector -> 'a

    val findi  : (int * elem -> bool) -> vector -> (int * elem) option
    val find   : (elem -> bool) -> vector -> elem option
    val exists : (elem -> bool) -> vector -> bool
    val all    : (elem -> bool) -> vector -> bool
    val collate: (elem * elem -> order) -> vector * vector -> order

    (* Basis Library proposal 2015-003 *)
    val toList  : vector -> elem list
    val append  : vector * elem -> vector
    val prepend : elem * vector -> vector

  end
(******************** ../BASIS/char-vector.sml ********************)
(* char-vector.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure CharVector : MONO_VECTOR =
  struct

    structure V = Unsafe.CharVector

    (* fast add/subtract avoiding the overflow test *)
    infix -- ++
    fun x -- y = Word.toIntX(Word.fromInt x - Word.fromInt y)
    fun x ++ y = Word.toIntX(Word.fromInt x + Word.fromInt y)

    fun uLessThan (x, y) = Word.<(Word.fromInt x, Word.fromInt y)

  (* unchecked access operations *)
    val usub = V.sub
    val uupd = V.update

    type vector = V.vector
    type elem = V.elem

    val maxLen = CharVector.maxLen

    val vector0 : vector = V.create 0

    fun createVec n = if uLessThan(maxLen, n)
	  then raise Size
	  else V.create n

    fun fromList [] = vector0
      | fromList vl = let
          val len = let
                fun lp ([], n) = n
                  | lp (_::r, n) = lp (r, n ++ 1)
                in
                  lp (vl, 0)
                end
	  val v = createVec len
	  fun copy ([], _) = ()
	    | copy (b::r, i) = (uupd(v, i, b); copy(r, i++1))
	  in
	    copy (vl, 0); v
	  end

    fun tabulate (0, _) = vector0
      | tabulate (n, f) = let
	  val ss = createVec n
	  fun fill i =
	      if i < n then (uupd (ss, i, f i); fill (i ++ 1))
	      else ()
	  in
	    fill 0; ss
	  end

    val length = CharVector.length
    val sub = CharVector.sub

    fun concat [] = vector0
      | concat [s] = s
      | concat (sl : vector list) = let
        (* compute total length of the result string *)
          fun len (i, []) = i
            | len (i, s::rest) = len(i+length s, rest)
          in
            case len (0, sl)
             of 0 => vector0
              | 1 => let
                  fun find (v :: r) = if length v = 0 then find r else v
                    | find _ = vector0 (** impossible **)
                  in
                    find sl
                  end
              | totLen => let
                  val v = createVec totLen
                  fun copy ([], _) = ()
                    | copy (s::r, i) = let
                        val len = length s
                        fun copy' j =
                            if (j = len) then ()
                            else (uupd(v, i++j, usub(s, j)); copy'(j++1))
                        in
                          copy' 0;
                          copy (r, i++len)
                        end
                  in
                    copy (sl, 0);
                    v
                  end
            (* end case *)
          end (* concat *)

    fun appi f vec = let
	val len = length vec
	fun app i =
	    if i >= len then () else (f (i, usub (vec, i)); app (i ++ 1))
        in
          app 0
        end

    fun app f vec = let
	val len = length vec
	fun app i =
	    if i >= len then () else (f (usub (vec, i)); app (i ++ 1))
        in
          app 0
        end

    val update = CharVector.update

    fun mapi f vec = tabulate (length vec, fn i => f (i, usub (vec, i)))

    fun map f vec = (case (length vec)
	   of 0 => vector0
	    | len => let
		val newVec = V.create len
		fun mapf i = if (i < len)
		      then (uupd(newVec, i, f(usub(vec, i))); mapf(i+1))
		      else ()
		in
		  mapf 0; newVec
		end
	  (* end case *))

    fun foldli f init vec = let
	val len = length vec
	fun fold (i, a) =
	    if i >= len then a else fold (i ++ 1, f (i, usub (vec, i), a))
        in
          fold (0, init)
        end

    fun foldl f init vec = let
	val len = length vec
	fun fold (i, a) =
	    if i >= len then a else fold (i ++ 1, f (usub (vec, i), a))
        in
          fold (0, init)
        end

    fun foldri f init vec = let
	fun fold (i, a) =
	    if i < 0 then a else fold (i --1, f (i, usub (vec, i), a))
        in
	  fold (length vec -- 1, init)
        end

    fun foldr f init vec = let
	fun fold (i, a) =
	    if i < 0 then a else fold (i --1, f (usub (vec, i), a))
        in
	  fold (length vec -- 1, init)
        end

    fun findi p vec = let
	val len = length vec
	fun fnd i =
	    if i >= len then NONE
	    else let val x = usub (vec, i)
		 in
		     if p (i, x) then SOME (i, x) else fnd (i ++ 1)
		 end
        in
          fnd 0
        end

    fun find p vec = let
	val len = length vec
	fun fnd i =
	    if i >= len then NONE
	    else let val x = usub (vec, i)
		 in
		     if p x then SOME x else fnd (i ++ 1)
		 end
        in
          fnd 0
        end

    fun exists p vec = let
	val len = length vec
	fun ex i = i < len andalso (p (usub (vec, i)) orelse ex (i ++ 1))
        in
          ex 0
        end

    fun all p vec = let
	val len = length vec
	fun al i = i >= len orelse (p (usub (vec, i)) andalso al (i ++ 1))
        in
          al 0
        end

    fun collate c (v1, v2) = let
	val l1 = length v1
	val l2 = length v2
	val l12 = Int.min (l1, l2)
	fun col i =
	    if i >= l12 then Int.compare (l1, l2)
	    else (case c (usub (v1, i), usub (v2, i))
		  of EQUAL => col (i ++ 1)
		   | unequal => unequal)
        in
          col 0
        end

    (* added for Basis Library proposal 2015-003 *)
    local
    (* utility function for extracting the elements of a vector as a list *)
      fun getList (_, 0, l) = l
	| getList (vec, i, l) = let val i = i -- 1
	    in
	      getList (vec, i, usub(vec, i) :: l)
	    end
    in

    fun toList vec = let
	  val n = length vec
	  in
	    getList (vec, n, [])
	  end

    fun append (vec, x) = let
	  val n = length vec
	  val n' = n ++ 1
	  val ss = createVec n'
	  fun fill i = if i < n
		then (uupd (ss, i, usub(vec, i)); fill (i ++ 1))
	        else ()
	  in
	    fill 0; uupd (ss, n, x);
	    ss
	  end

    fun prepend (x, vec) = let
	  val n = length vec
	  val n' = n ++ 1
	  val ss = createVec n'
	  fun fill i = if i < n
		then (uupd (ss, i ++ 1, usub(vec, i)); fill (i ++ 1))
	        else ()
	  in
	    uupd (ss, 0, x); fill 0;
	    ss
	  end

    end (* local *)

  end
(******************** stream.sml ********************)
(* stream.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Stream =
  struct

    datatype 'a u = Nil | Cons of 'a * 'a t
    withtype 'a t = unit -> 'a u

    fun unfold (f : 'b -> ('a * 'b) option) : 'b -> 'a t = let
          fun loop b () = (case f b
                of NONE => Nil
                 | SOME (x, b) => Cons (x, loop b))
          in
            loop
          end

    fun map (f : 'a -> 'b) : 'a t -> 'b t =
          unfold (fn s => case s () of Nil => NONE | Cons (x, xs) => SOME (f x, xs))

  end
(******************** pi-digits.sml ********************)
(* pi-digits.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure PiDigits : sig

    val pi : IntInf.int Stream.t

  end = struct

    fun stream {
            next : 'b -> 'c,
            safe : 'b -> 'c -> bool,
            prod : 'b -> 'c -> 'b,
            cons : 'b -> 'a -> 'b
          } : 'b -> 'a Stream.t -> 'c Stream.t = let
          fun loop z s () = let
                val y = next z
                in
                  if safe z y
                    then Stream.Cons (y, loop (prod z y) s)
                    else (case s ()
                       of Stream.Nil => Stream.Nil
                        | Stream.Cons (x, xs) => loop (cons z x) xs ())
                end
          in
            loop
          end

    type lft = (IntInf.int * IntInf.int * IntInf.int * IntInf.int)

    val unit : lft = (1,0,0,1)

    fun comp (q,r,s,t) (u,v,w,x) : lft = (q*u+r*w, q*v+r*x, s*u+t*w, s*v+t*x)

    val pi = let
          val init = unit
          val lfts = Stream.map (fn k => (k, 4*k+2, 0, 2*k+1)) (Stream.unfold (fn i => SOME (i, i+1)) 1)
          fun floor_extr (q,r,s,t) x = (q * x + r) div (s * x + t)
          fun next z = floor_extr z 3
          fun safe z n = n = floor_extr z 4
          fun prod z n = comp (10, ~10*n, 0, 1) z
          fun cons z z' = comp z z'
          in
            stream {next = next, safe = safe, prod = prod, cons = cons} init lfts
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

    fun display n = let
          fun loop (ds, (k, col)) = if k < n
                then let
                  val col = if col = 10
                        then (Log.say["\t:", IntInf.toString k, "\n"]; 1)
                        else col + 1
                  in
                    case ds ()
                     of Stream.Nil => raise Empty
                      | Stream.Cons(d, ds) => (
                          Log.print (IntInf.toString d);
                          loop (ds, (k + 1, col)))
                  end
                else Log.say [
                    CharVector.tabulate (10 - col, fn _ => #" "),
                    "\t:", IntInf.toString k, "\n"
                  ]
          in
            loop (PiDigits.pi, (0, 0))
          end

    fun testit () = display 30

    fun doit () = display 2000

  end
end

(* all.sml -- all sources for minimax *)
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
(******************** ../BASIS/list.sig ********************)
(* list.sig
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Available (unqualified) at top level:
 *   type list
 *   val nil, ::, hd, tl, null, length, @, app, map, foldr, foldl, rev
 *
 * Consequently the following are not visible at top level:
 *   val last, nth, take, drop, concat, revAppend, mapPartial, find, filter,
 *       partition, exists, all, tabulate
 *   exception Empty
 *
 * The following infix declarations will hold at top level:
 *   infixr 5 :: @
 *
 *)

signature LIST_2004 =
  sig

(*    datatype 'a list = nil | :: of ('a * 'a list) *)
    datatype list = datatype list

    exception Empty

    val null : 'a list -> bool 
    val hd   : 'a list -> 'a                (* raises Empty *)
    val tl   : 'a list -> 'a list           (* raises Empty *)
    val last : 'a list -> 'a                (* raises Empty *)

    val getItem : 'a list -> ('a * 'a list) option

    val nth  : 'a list * int -> 'a       (* raises Subscript *)
    val take : 'a list * int -> 'a list  (* raises Subscript *)
    val drop : 'a list * int -> 'a list  (* raises Subscript *)

    val length : 'a list -> int 

    val rev : 'a list -> 'a list 

    val @         : 'a list * 'a list -> 'a list
    val concat    : 'a list list -> 'a list
    val revAppend : 'a list * 'a list -> 'a list

    val app        : ('a -> unit) -> 'a list -> unit
    val map        : ('a -> 'b) -> 'a list -> 'b list
    val mapPartial : ('a -> 'b option) -> 'a list -> 'b list

    val find      : ('a -> bool) -> 'a list -> 'a option
    val filter    : ('a -> bool) -> 'a list -> 'a list
    val partition : ('a -> bool ) -> 'a list -> ('a list * 'a list)

    val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b

    val exists : ('a -> bool) -> 'a list -> bool
    val all    : ('a -> bool) -> 'a list -> bool

    val tabulate : (int * (int -> 'a)) -> 'a list   (* raises Size *)

    val collate : ('a * 'a -> order) -> 'a list * 'a list -> order

  end (* signature LIST *)

(* includes Basis Library proposal 2015-003 *)
signature LIST_2015 =
  sig

    include LIST_2004

    val unfoldl        : ('strm -> ('a * 'strm) option) -> 'strm -> 'a list
    val unfoldr        : ('strm -> ('a * 'strm) option) -> 'strm -> 'a list

    val reduce         : ('a * 'a -> 'a) -> 'a -> 'a list -> 'a

    val appi		: (int * 'a -> unit) -> 'a list -> unit
    val mapi		: (int * 'a -> 'b) -> 'a list -> 'b list
    val mapPartiali	: (int * 'a -> 'b option) -> 'a list -> 'b list
    val foldli		: (int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val foldri		: (int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val findi		: (int * 'a -> bool) -> 'a list -> (int * 'a) option

    val revMap		: ('a -> 'b) -> 'a list -> 'b list
    val revMapi		: (int * 'a -> 'b) -> 'a list -> 'b list
    val revMapPartial	: ('a -> 'b option) -> 'a list -> 'b list
    val revMapPartiali	: (int * 'a -> 'b option) -> 'a list -> 'b list

    val concatMap	: ('a -> 'b list) -> 'a list -> 'b list
    val concatMapi	: (int * 'a -> 'b list) -> 'a list -> 'b list

    val foldMapl	: ('b * 'c -> 'c) -> ('a -> 'b) -> 'c -> 'a list -> 'c
    val foldMapr	: ('b * 'c -> 'c) -> ('a -> 'b) -> 'c -> 'a list -> 'c

    val splitAt		: 'a list * int -> 'a list * 'a list
    val update		: 'a list * int * 'a -> 'a list
    val sub		: 'a list * int -> 'a

  end

signature LIST = LIST_2015

(* top-level bindings *)
datatype list = datatype List.list
val hd = List.hd
val tl = List.tl
val null = List.null
val length = List.length
val map = List.map
val foldr = List.foldr
val foldl = List.foldl
val rev = List.rev

(******************** ../BASIS/list.sml ********************)
(* list.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Available (unqualified) at top level:
 *   type list
 *   val nil, ::, hd, tl, null, length, @, app, map, foldr, foldl, rev
 *   exception Empty
 *
 *)

structure List : LIST =
  struct

    (* fast add/subtract avoiding the overflow test *)
    infix 6 -- ++
    fun x -- y = Word.toIntX(Word.fromInt x - Word.fromInt y)
    fun x ++ y = Word.toIntX(Word.fromInt x + Word.fromInt y)

    datatype list = datatype list

    exception Empty = Empty

    fun null [] = true
      | null _ = false
    fun hd (h :: _) = h
      | hd [] = raise Empty
    fun tl (_ :: t) = t
      | tl [] = raise Empty
    fun length l = let
          fun loop (n, []) = n
            | loop (n, [_]) = n ++ 1
            | loop (n, _ :: _ :: l) = loop (n ++ 2, l)
          in
            loop (0, l)
          end
    fun revAppend ([], l) = l
      | revAppend (x::r, l) = revAppend(r, x::l)
    fun rev l = revAppend(l, [])
    fun l1 @ l2 = revAppend(rev l1, l2)
    fun foldr f b l = foldl f b (rev l)
    fun foldl f b l = let
          fun f2 ([], b) = b
            | f2 (a :: r, b) = f2 (r, f (a, b))
          in
            f2 (l, b)
          end
    fun app f = let
          fun a2 [] = ()
            | a2 (h :: t) = (f h : unit; a2 t)
          in
            a2
          end
    fun map f = let
          fun m [] = []
            | m [a] = [f a]
            | m [a, b] = [f a, f b]
            | m [a, b, c] = [f a, f b, f c]
            | m (a :: b :: c :: d :: r) = f a :: f b :: f c :: f d :: m r
          in
            m
          end

    fun last [] = raise Empty
      | last [x] = x
      | last (_::r) = last r

    fun getItem [] = NONE
      | getItem (x::r) = SOME(x, r)

    fun nth (l, n) = let
          fun loop ((e::_),0) = e
            | loop ([],_) = raise Subscript
            | loop ((_::t),n) = loop(t, n -- 1)
          in
            if n >= 0 then loop (l,n) else raise Subscript
          end

    fun take (l, n) = let
          fun loop (l, 0) = []
            | loop ([], _) = raise Subscript
            | loop ((x::t), n) = x :: loop (t, n -- 1)
          in
            if n >= 0 then loop (l, n) else raise Subscript
          end

    fun drop (l, n) = let
          fun loop (l,0) = l
            | loop ([],_) = raise Subscript
            | loop ((_::t),n) = loop(t, n -- 1)
          in
            if n >= 0 then loop (l,n) else raise Subscript
          end

    fun concat [] = []
      | concat (l::r) = l @ concat r

    fun mapPartial pred l = let
          fun mapp ([], l) = rev l
            | mapp (x::r, l) = (case (pred x)
                 of SOME y => mapp(r, y::l)
                  | NONE => mapp(r, l)
                (* end case *))
          in
            mapp (l, [])
          end

    fun find pred [] = NONE
      | find pred (a::rest) = if pred a then SOME a else (find pred rest)

    fun filter pred [] = []
      | filter pred (a::rest) = if pred a
	  then a::(filter pred rest)
	  else (filter pred rest)

    fun partition pred l = let
          fun loop ([],trueList,falseList) = (rev trueList, rev falseList)
            | loop (h::t,trueList,falseList) =
                if pred h then loop(t, h::trueList, falseList)
                else loop(t, trueList, h::falseList)
          in
	    loop (l,[],[])
	  end

    fun exists pred = let
          fun f [] = false
            | f (h::t) = pred h orelse f t
          in f end
    fun all pred = let
          fun f [] = true
            | f (h::t) = pred h andalso f t
          in f end

    fun tabulate (len, genfn) =
          if len < 0 then raise Size
          else let
            fun loop n = if n = len then []
                         else (genfn n)::(loop(n ++ 1))
            in loop 0 end

    fun collate compare = let
	  fun loop ([], []) = EQUAL
	    | loop ([], _) = LESS
	    | loop (_, []) = GREATER
	    | loop (x :: xs, y :: ys) = (case compare (x, y)
		 of EQUAL => loop (xs, ys)
		  | unequal => unequal)
	  in
	    loop
	  end

  (* added for Basis Library proposal 2015-003 *)

    fun unfoldr getNext strm = let
	  fun lp (strm, items) = (case getNext strm
		 of NONE => items
		  | SOME(x, rest) => lp(rest, x::items)
		(* end case *))
	  in
	    lp (strm, [])
	  end

    fun unfoldl getNext strm = rev(unfoldr getNext strm)

    fun reduce f id [] = id
      | reduce f _ (x::xs) = foldl f x xs

    fun appi f l = let
	  fun appf (_, []) = ()
	    | appf (i, x::xs) = (f(i, x); appf(i ++ 1, xs))
	  in
	    appf (0, l)
	  end

    fun mapi f l = let
	  fun mapf (_, []) = []
	    | mapf (i, x::xs) = (f(i, x) :: mapf(i ++ 1, xs))
	  in
	    mapf (0, l)
	  end

    fun mapPartiali pred l = let
          fun mapp (_, [], l) = rev l
            | mapp (i, x::r, l) = (case pred(i, x)
                 of SOME y => mapp(i ++ 1, r, y::l)
                  | NONE => mapp(i ++ 1, r, l)
                (* end case *))
          in
            mapp (0, l, [])
          end

    fun foldli f init l = let
          fun lp (_, [], acc) = acc
	    | lp (i, x::xs, acc) = lp (i ++ 1, xs, f(i, x, acc))
	  in
	    lp (0, l, init)
	  end

    fun foldri f init l = let
          fun lp (_, []) = init
	    | lp (i, x::xs) = f (i, x, lp (i ++ 1, xs))
	  in
	    lp (0, l)
	  end

    fun findi f l = let
	  fun lp (_, []) = NONE
	    | lp (i, x::xs) = if (f(i, x)) then SOME(i, x) else lp(i ++ 1, xs)
	  in
	    lp (0, l)
	  end

    fun revMap f l = let
	  fun mapf (x::xs, ys) = mapf (xs, f x :: ys)
	    | mapf ([], ys) = ys
	  in
	    mapf (l, [])
	  end
    fun revMapi f l = let
	  fun mapf (i, x::xs, ys) = mapf (i ++ 1, xs, f(i, x) :: ys)
	    | mapf (_, [], ys) = ys
	  in
	    mapf (0, l, [])
	  end

    fun revMapPartial pred l = let
          fun mapp ([], l) = l
            | mapp (x::r, l) = (case (pred x)
                 of SOME y => mapp(r, y::l)
                  | NONE => mapp(r, l)
                (* end case *))
          in
            mapp (l, [])
          end
    fun revMapPartiali pred l = let
          fun mapp (_, [], l) = l
            | mapp (i, x::r, l) = (case pred(i, x)
                 of SOME y => mapp(i ++ 1, r, y::l)
                  | NONE => mapp(i ++ 1, r, l)
                (* end case *))
          in
            mapp (0, l, [])
          end

    fun concatMap f l = let
	  fun mapf ([], l) = rev l
	    | mapf (x::r, l) = mapf (r, revAppend(f x, l))
	  in
	    mapf (l, [])
	  end
    fun concatMapi f l = let
	  fun mapf (_, [], l) = rev l
	    | mapf (i, x::r, l) = mapf (i ++ 1, r, revAppend(f(i, x), l))
	  in
	    mapf (0, l, [])
	  end

    fun foldMapl reduceFn mapFn init l = let
	  fun foldf ([], acc) = acc
	    | foldf (x::xs, acc) = foldf (xs, reduceFn(mapFn x, acc))
	  in
	    foldf (l, init)
	  end

    fun foldMapr reduceFn mapFn init l =
	  foldr (fn (x, acc) => reduceFn(mapFn x, acc)) init l

    fun splitAt (l, n) = let
          fun loop (0, xs, prefix) = (rev prefix, xs)
            | loop (_, [], _) = raise Subscript
            | loop (i, x::xs, prefix) = loop (i -- 1, xs, x::prefix)
          in
            if n >= 0 then loop (n, l, []) else raise Subscript
          end

    fun update (l, n, y) = let
	  fun upd (0, x::xs, prefix) = revAppend(prefix, y::xs)
	    | upd (_, [], _) = raise Subscript
	    | upd (i, x::xs, prefix) = upd (i -- 1, xs, x::prefix)
	  in
	    if (n < 0) then raise Subscript else upd(n, l, [])
	  end

    val sub = nth

  end (* structure List *)

(* top-level bindings for List operations *)
val null = List.null
val hd = List.hd
val tl = List.tl
val length = List.length
val op @ = List.@
val rev = List.rev
val app = List.app
val map = List.map
val foldl = List.foldl
val foldr = List.foldr
(******************** ../BASIS/option.sig ********************)
(* option.sig
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature OPTION_2004 =
  sig
    datatype 'a option = NONE | SOME of 'a

    exception Option

    val getOpt         : ('a option * 'a) -> 'a
    val isSome         : 'a option -> bool
    val valOf          : 'a option -> 'a
    val filter         : ('a -> bool) -> 'a -> 'a option
    val join           : 'a option option -> 'a option
    val app            : ('a -> unit) -> 'a option -> unit
    val map            : ('a -> 'b) -> 'a option -> 'b option
    val mapPartial     : ('a -> 'b option) -> 'a option -> 'b option
    val compose        : (('a -> 'b) * ('c -> 'a option)) -> 'c -> 'b option
    val composePartial : (('a -> 'b option) * ('c -> 'a option)) -> 'c -> 'b option

  end;

(* added for Basis Library proposal 2015-003 *)
signature OPTION_2015 =
  sig

    include OPTION_2004

    val isNone : 'a option -> bool
    val fold : ('a * 'b -> 'b) -> 'b -> 'a option -> 'b

  end

signature OPTION = OPTION_2015
(******************** ../BASIS/option.sml ********************)
(* option.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Option : OPTION =
  struct

    datatype option = datatype Option.option

    exception Option = Option

    fun getOpt (SOME x, y) = x
      | getOpt (NONE, y) = y
    fun isSome (SOME _) = true
      | isSome NONE = false
    fun valOf (SOME x) = x
      | valOf _ = raise Option

    fun filter pred x = if (pred x) then SOME x else NONE
    fun join (SOME opt) = opt
      | join NONE = NONE
    fun app f (SOME x) = f x
      | app f NONE = ()
    fun map f (SOME x) = SOME(f x)
      | map f NONE = NONE
    fun mapPartial f (SOME x) = f x
      | mapPartial f NONE = NONE
    fun compose (f, g) x = map f (g x)
    fun composePartial (f, g) x = mapPartial f (g x)

  (* added for Basis Library proposal 2015-003 *)
    fun isNone NONE = true
      | isNone _ = false

    fun fold f init NONE = init
      | fold f init (SOME x) = f(x, init);

  end

(* top-level bindings *)
datatype option = datatype Option.option
val getOpt = Option.getOpt
val isSome = Option.isSome
val valOf = Option.valOf
(******************** tic-tac-toe.sml ********************)
(* tic-tac-toe.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure TicTacToe : sig

    datatype player = X | O

    type board = player option list (* of length 9 *)

    datatype 'a rose_tree (* general tree *)
      = Rose of 'a * ('a rose_tree list)

    type game_tree = (board * int) rose_tree

    val empty : board

    val boardToIndex : player * board -> int

    val gameTree : unit -> game_tree

    val gameTreeWithTT : unit -> game_tree

  end = struct

    val hd = List.hd
    val tl = List.tl

    fun snd (_, y) = y

    datatype player = X | O

    type board = player option list (* of length 9 *)

    (* 0 1 2
       3 4 5
       6 7 8 *)
    val rows  = [[0,1,2], [3,4,5], [6,7,8]]
    val cols  = [[0,3,6], [1,4,7], [2,5,8]]
    val diags = [[0,4,8], [2,4,6]]

    val empty : board = List.tabulate (9, fn _ => NONE)

    (* isOccupied : board * int -> bool *)
    fun isOccupied (b,i) = Option.isSome(List.nth(b, i))

    (* isEmpty : board * int -> bool *)
    fun isEmpty (b, i) = Option.isNone (List.nth(b, i))

    (* find : board * int -> player option *)
    fun find (b, i) = List.nth (b, i)

    (* playerEq : player * player -> bool *)
    fun playerEq (p1, p2) = (case (p1, p2)
           of (X, X) => true
            | (O, O) => true
            | _ => false
          (* end case *))

    (* playerOccupies : player -> board -> int -> bool *)
    fun playerOccupies p b i = (case find (b, i)
	     of SOME p' => playerEq (p, p')
	      | NONE => false
          (* end case *))

    (* putAt : 'a * 'a list * int -> 'a list *)
    fun putAt (x, xs, i) =
	  if (i=0)
            then x :: tl(xs)
	  else if (i>0)
            then hd xs :: putAt(x, tl(xs), i-1)
	    else (* i<0 *) raise Fail "subscript"

    (* moveTo : (board * player) -> int -> board *)
    fun moveTo (b : board, p : player) (i:int) =
	  if isOccupied(b,i)
	    then raise Fail "illegal move"
	    else putAt (SOME(p), b, i)

    (* hasTrip : board * player -> int list -> bool *)
    fun hasTrip (b, p) t = List.all (playerOccupies p b) t

    (* hasRow : board * player -> bool *)
    fun hasRow (b, p) = List.exists (hasTrip (b, p)) rows

    (* hasCol : board * player -> bool *)
    fun hasCol (b, p) = List.exists (hasTrip (b, p)) cols

    (* hasDiag : board * player -> bool *)
    fun hasDiag (b, p) = List.exists (hasTrip (b, p)) diags

    (* isFull : board -> bool *)
    fun isFull b = List.all isSome b

    (* isWinFor : board -> player -> bool *)
    fun isWinFor b p = hasRow(b,p) orelse hasCol(b,p) orelse hasDiag(b,p)

    (* isWin : board -> bool *)
    fun isWin b = isWinFor b X orelse isWinFor b O

    (* isCat : board -> bool *)
    fun isCat b = isFull b andalso not (isWinFor b X)
			   andalso not (isWinFor b O) (* X moves last *)

    (* score : board -> int *)
    (* -1 if O wins, 1 if X wins, 0 otherwise. *)
    (* This coarse heuristic function suffices b/c we can build the *whole* tree. *)
    fun score b = if isWinFor b X then 1 else if isWinFor b O then ~1 else 0

    datatype 'a rose_tree (* general tree *)
      = Rose of 'a * ('a rose_tree list)

    (* mkLeaf : 'a -> 'a rose_tree *)
    fun mkLeaf x = Rose (x, nil)

    type game_tree = (board * int) rose_tree

    (* allMoves : board -> int list *)
    fun allMoves b =
	let fun f (n, m, acc) =
		(case m
		  of nil => rev acc
		   | SOME(_)::more => f (n+1, more, acc)
		   | NONE::more => f (n+1, more, n::acc))
	in
	    f (0, b, nil)
	end

    (* other : player -> player *)
    fun other p = (case p
		    of X => O
		     | O => X)

    (* top : 'a rose_tree -> 'a *)
    fun top (Rose (x, _)) = x

    (* size : 'a rose_tree -> int *)
    fun size (Rose (x, ts)) = 1 + (foldl (fn (x,y) => x+y) 0 (map size ts))

    (* listExtreme : (('a * 'a) -> 'a) -> 'a list -> 'a *)
    fun listExtreme e [] = raise List.Empty
      | listExtreme e (n::ns) = List.foldl e n ns

    (* listmax : int list -> int *)
    val listmax = listExtreme (fn (a:int, b) => if (a>b) then a else b)

    (* listmax : int list -> int *)
    val listmin = listExtreme (fn (a:int, b) => if (a<b) then a else b)

    (* gameOver : board -> bool *)
    fun gameOver b = isWin b orelse isCat b

    (* successors : board * player -> board list *)
    (* A list of all possible successor states given a board and a player to move. *)
    fun successors (b : board, p : player) : board list = map (moveTo (b, p)) (allMoves b)

    (* minimax : player -> board -> game_tree *)
    (* Build the tree and score it at the same time. *)
    (* p is the player to move *)
    (* X is max, O is min *)
    fun minimax (p : player) (b : board) : game_tree = if gameOver b
          then mkLeaf (b, score b)
	  else let
	    val trees = map (minimax (other p)) (successors (b, p))
	    val scores = map (snd o top) trees
	    in
	       case p
		of X => Rose ((b, listmax scores), trees)
		 | Y => Rose ((b, listmin scores), trees)
              (* end case *)
	    end

    (* construct the game tree starting with an empty board *)
    fun gameTree () = minimax X empty

    (* map a board to a unique integer *)
    fun boardToIndex (p, b) = let
          fun handleItem i = (case i
                 of SOME X => 1
                  | SOME O => 2
                  | NONE => 0
                (* end case *))
          val (res, _) = List.foldr
                (fn (i, (sum, base)) => (sum + handleItem i * base, base*3))
                (case p of X => 1 | _ => 0, 3)
                b
          in
            res
          end

    (* construct the game tree starting with an empty board using a transposition
     * table.
     *)
    fun gameTreeWithTT () = let
          (* Transposition Table *)
          val unused = ~2
          (* 3^10 = 59,049 *)
          val transTable = Array.tabulate (59049, fn _ => unused)
          fun lookupTrans (p, b) = (case Array.sub (transTable, boardToIndex (p, b))
                 of ~2 => NONE
                  | x => SOME x
                (* end case *))
          fun setTrans (p, b, i) =
                Array.update (transTable, boardToIndex (p, b), i)
          fun minimaxTT (p : player) (b : board) : game_tree = if gameOver b
                then mkLeaf (b, score b)
                else (case lookupTrans (p,b)
                   of SOME x => Rose ((b, x), [])
                    | NONE => let
                        val trees = map (minimaxTT (other p)) (successors (b, p))
                        val scores = map (snd o top) trees
                        val final = (case p
                               of X => listmax scores
                                | Y => listmin scores
                              (* end case *))
                        in
                          setTrans (p, b, final);
                          Rose ((b, final), trees)
                        end
                  (* end case *))
          in
            minimaxTT X empty
          end

  end
in
(******************** main.sml ********************)
(* main.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    structure T = TicTacToe

    val name = "minimax"

    val results : string list = []

    val iters = 10

    fun doit () = let
	    fun doit () = (
                  T.gameTree();
                  T.gameTreeWithTT())
            fun loop n = if n = 0
                  then ()
                  else (doit () ; loop (n-1))
            in
              loop iters
            end

    (* print the size of the tree *)
    fun printTree tr = let
          fun sz (T.Rose(_, kids)) = let
                fun doKid (t, (n, d)) = let
                      val (n', d') = sz t
                      in
                        (n+n', Int.max(d, d'))
                      end
                val (n, d) = List.foldl doKid (0, 0) kids
                in
                  (n+1, d+1)
                end
          val (n, d) = sz tr
          in
            Log.say [
                "# nodes = ", Int.toString n, ", max depth = ", Int.toString d, "\n"
              ]
          end

    fun testit () = (
          Log.print "Minimax: ";
          printTree (T.gameTree());
          Log.print "Minimax+TT: ";
          printTree (T.gameTreeWithTT()))

  end
end

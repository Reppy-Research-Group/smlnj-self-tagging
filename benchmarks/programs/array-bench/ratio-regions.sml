(* all.sml -- all sources for ratio-regions *)
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

    val name = "ratio-regions"

    val results : string list = []

    fun doo (max: int, f: int -> unit): unit = let
        fun loop i = if i >= max then () else (f i; loop(i + 1))
        in
          loop 0
        end

    fun zero x = x = 0
    val cons = op ::
    fun write_char c = () (* TextIO.output1(TextIO.stdOut, c) *)
    val modulo = Int.mod
    val quotient = Int.quot
    fun for_each(l, f) = List.app f l
    fun negative x = x < 0
    fun positive x = x > 0

    fun min l = (case l
         of x :: l => let
            fun loop(l, min) = (case l
                 of [] => min
                  | x :: l => loop(l, Int.min(min, x)))
            in
              loop(l, x)
            end
          | _ => raise Fail "min"
        (* end case *))

    fun every_n(n, p) = let
          fun loop i = i >= n orelse (p i andalso loop(i + 1))
          in
            loop 0
          end

    fun some(l, p) = List.exists p l

    fun some_n(n, p) = let
        fun loop i = i < n andalso (p i orelse loop(i + 1))
        in
          loop 0
        end

    fun x (x, _) = x
    fun y (_, y) = y

    datatype 'a matrix = Matrix of 'a array array

    fun make_matrix(m: int, n: int, a: 'a): 'a matrix =
        Matrix(A.tabulate(m, fn i => A.array(n, a)))

    fun matrix_rows(Matrix a) = A.length a
    fun matrix_columns(Matrix a) = A.length(A.sub(a, 0))
    fun matrix_ref(Matrix a, i, j) = A.sub(A.sub(a, i), j)
    fun matrix_set(Matrix a, i, j, x) = A.update(A.sub(a, i), j, x)

    datatype formatValue = Int of int | String of string

    fun format (control_string: string, values: formatValue list): unit = let
        fun loop(i: int, values: formatValue list): unit =
           if not(i = String.size control_string)
              then
                 let val c = String.sub(control_string, i)
                 in if c = #"~"
                       then let val c2 = String.sub(control_string, i + 1)
                            in case (c2, values) of
                               (#"s", Int n :: values) =>
                                  (Log.print(Int.toString n) ; loop(i + 2, values))
                             | (#"a", String s :: values) =>
                                  (Log.print s ; loop(i + 2, values))
                             | (#"%", _) =>
                                  (Log.print "\n"; loop(i + 2, values))
                             | _ => (write_char c; loop(i + 1, values))
                            end
                    else (write_char c ; loop(i + 1, values))
                 end
           else ()
        in loop(0, values)
        end

(* The vertices are s, t, and (y,x).
 * C_RIGHT[y,x] is the capacity from (y,x) to (y,x+1) which is the same as the
 * capacity from (y,x+1) to (y,x).
 * C_DOWN[y,x] is the capacity from (y,x) to (y+1,x) which is the same as the
 * capacity from (y+1,x) to (y,x).
 * The capacity from s to (y,0), (0,x), (y,Y_1), (0,X_1) is implicitly
 * infinite.
 * The capacity from (x,y) to t is V*W[y,x].
 * F_RIGHT[y,x] is the preflow from (y,x) to (y,x+1) which is the negation of
 * the preflow from (y,x+1) to (y,x).
 * F_DOWN[y,x] is the preflow from (y,x) to (y+1,x) which is the negation of
 * the preflow from (y+1,x) to (y,x).
 * We do not record the preflow from s to (y,X_1), (y,0), (Y_1,x), and (0,x)
 * and from (y,X_1), (y,0), (Y_1,x), and (0,x) to s.
 * F_T[y,x] is the preflow from (y,x) to t.
 * We do not record the preflow from t to (y,x).
 * {C,F}_RIGHT[0:Y_1,0:X_2].
 * {C,F}_DOWN[0:Y_2,0:X_1].
 * F_T[0:Y_1,0:X_1]
 * For now, we will keep all capacities (and thus all preflows) as integers.
 * (CF_RIGHT y x) is the residual capacity from (y,x) to (y,x+1).
 * (CF_LEFT y x) is the residual capacity from (y,x) to (y,x_1).
 * (CF_DOWN y x) is the residual capacity from (y,x) to (y+1,x).
 * (CF_UP y x) is the residual capacity from (y,x) to (y_1,x).
 * We do not compute the residual capacities from s to (y,X_1), (y,0),
 * (Y_1,x), and (0,x) because they are all infinite.
 * We do not compute the residual capacities from (y,X_1), (y,0), (Y_1,x),
 * and (0,x) to s because they will never be used.
 * (CF_T y x) is the residual capacity from (y,x) to t.
 * We do not compute the residual capacity from t to (y,x) because it will
 * be used.
 * (EF_RIGHT? y x) is true if there is an edge from (y,x) to (y,x+1) in the
 * residual network.
 * (EF_LEFT? y x) is true if there is an edge from (y,x) to (y,x_1) in the
 * residual network.
 * (EF_DOWN? y x) is true if there is an edge from (y,x) to (y+1,x) in the
 * residual network.
 * (EF_UP? y x) is true if there is an edge from (y,x) to (y_1,x) in the
 * residual network.
 * (EF_T? y x) is true if there is an edge from (y,x) to t in the
 * residual network.
 * There are always edges in the residual network from s to (y,X_1), (y,0),
 * (Y_1,x), and (0,x).
 * We don't care whether there are edges in the residual network from
 * (y,X_1), (y,0), (Y_1,x), and (0,x) to s because they will never be used.
 * We don't care whether there are edges in the residual network from t to
 * (y,x) because they will never be used.
 *)

fun positive_min(x, y) = if negative x then y else Int.min(x, y)

fun positive_minus(x, y) = if negative x then x else x - y

fun positive_plus(x, y) = if negative x then x else x + y

fun rao_ratio_region(c_right, c_down, w, lg_max_v) =
   let val height = matrix_rows w
      val width  = matrix_columns w
      val f_right = make_matrix(height, width - 1, 0)
      val f_down = make_matrix(height - 1, width, 0)
      val f_t = make_matrix(height, width, 0)
      val h = make_matrix(height, width, 0)
      val e = make_matrix(height, width, 0)
      val marked = make_matrix(height, width, false)
      val m1 = height * width + 2
      val m2 = 2 * height * width + 2
      val q = A.array(2 * height * width + 3, [])
      fun cf_right(y, x) =
         matrix_ref(c_right, y, x) - matrix_ref(f_right, y, x)
      fun cf_left(y, x) =
         matrix_ref(c_right, y, x - 1) + matrix_ref(f_right, y, x - 1)
      fun cf_down(y, x) =
         matrix_ref(c_down, y, x) - matrix_ref(f_down, y, x)
      fun cf_up(y, x) =
         matrix_ref(c_down, y - 1, x) + matrix_ref(f_down, y - 1, x)
      fun ef_right(y, x) = positive(cf_right(y, x))
      fun ef_left(y, x) = positive(cf_left(y, x))
      fun ef_down(y, x) = positive(cf_down(y, x))
      fun ef_up(y, x) = positive(cf_up(y, x))
      fun preflow_push v =
         let
            fun enqueue(y, x) =
               if not(matrix_ref(marked, y, x))
                  then
                     (A.update(q,
                                 matrix_ref(h, y, x),
                                 (cons((x, y),
                                       A.sub(q, matrix_ref(h, y, x)))))
                      ; matrix_set(marked, y, x, true))
               else ()
            fun cf_t(y, x) = v * matrix_ref(w, y, x) - matrix_ref(f_t, y, x)
            fun ef_t(y, x) = positive(cf_t(y, x))
            fun can_push_right(y, x) =
               x < width - 1
               andalso not(zero(matrix_ref(e, y, x)))
               andalso ef_right(y, x)
               andalso matrix_ref(h, y, x) = matrix_ref(h, y, x + 1) + 1
            fun can_push_left(y, x) =
               x > 0
               andalso not(zero(matrix_ref(e, y, x)))
               andalso ef_left(y, x)
               andalso matrix_ref(h, y, x) = matrix_ref(h, y, x - 1) + 1
            fun can_push_down(y, x) =
               y < height - 1
               andalso not(zero(matrix_ref(e, y, x)))
               andalso ef_down(y, x)
               andalso matrix_ref(h, y, x) = matrix_ref(h, y + 1, x) + 1
            fun can_push_up(y, x) =
               y > 0
               andalso not(zero(matrix_ref(e, y, x)))
               andalso ef_up(y, x)
               andalso matrix_ref(h, y, x) = matrix_ref(h, y - 1, x) + 1
            fun can_push_t(y, x) =
               not(zero(matrix_ref(e, y, x)))
               andalso ef_t(y, x)
               andalso matrix_ref(h, y, x) = 1
            fun can_lift(y, x) =
               not(zero(matrix_ref(e, y, x)))
               andalso (if x = width - 1
                           then matrix_ref(h, y, x) <= m1
                        else (not(ef_right(y, x))
                              orelse
                              matrix_ref(h, y, x) <= matrix_ref(h, y, x + 1)))
               andalso (if x = 0
                           then matrix_ref(h, y, x) <= m1
                        else (not(ef_left(y, x))
                              orelse
                              matrix_ref(h, y, x) <= matrix_ref(h, y, x - 1)))
               andalso (if y = height - 1
                           then matrix_ref(h, y, x) <= m1
                        else (not(ef_down(y, x))
                              orelse
                              matrix_ref(h, y, x) <= matrix_ref(h, y + 1, x)))
               andalso (if y = 0
                           then matrix_ref(h, y, x) <= m1
                        else (not(ef_up(y, x))
                              orelse
                              matrix_ref(h, y, x) <= matrix_ref(h, y - 1, x)))
               andalso (not(ef_t(y, x)) orelse matrix_ref(h, y, x) = 0)
            fun push_right(y, x) =
               (* (format "Push right ~s ~s~%" y x) *)
               let val df_u_v = positive_min(matrix_ref(e, y, x), cf_right(y, x))
               in matrix_set(f_right, y, x, matrix_ref(f_right, y, x) + df_u_v)
                  ; matrix_set(e, y, x,
                               positive_minus(matrix_ref(e, y, x), df_u_v))
                  ; matrix_set(e, y, x + 1,
                               positive_plus(matrix_ref(e, y, x + 1), df_u_v))
                  ; enqueue(y, x + 1)
               end
            fun push_left(y, x) =
               (* (format "Push left ~s ~s~%" y x) *)
               let val df_u_v = positive_min(matrix_ref(e, y, x), cf_left(y, x))
               in matrix_set(f_right, y, x - 1,
                             matrix_ref(f_right, y, x - 1) - df_u_v)
                  ; matrix_set(e, y, x,
                               positive_minus(matrix_ref(e, y, x), df_u_v))
                  ; matrix_set(e, y, x - 1,
                               positive_plus(matrix_ref(e, y, x - 1), df_u_v))
                  ; enqueue(y, x - 1)
               end

            fun push_down(y, x) =
               (* (format "Push down ~s ~s~%" y x) *)
               let val df_u_v = positive_min(matrix_ref(e, y, x), cf_down(y, x))
               in matrix_set(f_down, y, x, matrix_ref(f_down, y, x) + df_u_v)
                  ; matrix_set(e, y, x,
                               positive_minus(matrix_ref(e, y, x), df_u_v))
                  ; matrix_set(e, y + 1, x,
                               positive_plus(matrix_ref(e, y + 1, x), df_u_v))
                  ; enqueue(y + 1, x)
               end
            fun push_up(y, x) =
               (* ;;(format "Push up ~s ~s~%" y x) *)
               let val df_u_v = positive_min(matrix_ref(e, y, x), cf_up(y, x))
               in matrix_set(f_down, y - 1, x,
                             matrix_ref(f_down, y - 1, x) - df_u_v)
                  ; matrix_set(e, y, x,
                               positive_minus(matrix_ref(e, y, x), df_u_v))
                  ; matrix_set(e, y - 1, x,
                               positive_plus(matrix_ref(e, y - 1, x), df_u_v))
                  ; enqueue(y - 1, x)
               end
            fun push_t(y, x) =
               (* ;;(format "Push t ~s ~s~%" y x) *)
               let val df_u_v = positive_min(matrix_ref(e, y, x), cf_t(y, x))
               in matrix_set(f_t, y, x, matrix_ref(f_t, y, x) + df_u_v)
                  ; matrix_set(e, y, x,
                               positive_minus(matrix_ref(e, y, x), df_u_v))
               end
            fun lift(y, x) =
               (* ;;(format "Lift ~s ~s~%" y x) *)
               matrix_set
               (h, y, x,
                1 + min[if x = width - 1
                           then m1
                        else if ef_right(y, x)
                                then matrix_ref(h, y, x + 1)
                             else m2,
                        if x = 0
                           then m1
                        else if ef_left(y, x)
                                then matrix_ref(h, y, x - 1)
                             else m2,
                        if y = height - 1
                           then m1
                        else if ef_down(y, x)
                                then matrix_ref(h, y + 1, x)
                             else m2,
                        if y = 0
                           then m1
                        else if ef_up(y, x)
                                then matrix_ref(h, y - 1, x)
                             else m2,
                        if ef_t(y, x) then 0 else m2])
            fun relabel() =
               (* ;;(format "Relabel~%") *)
               let
                  datatype 'a queue =
                     Nil
                   | Cons of 'a * 'a queue ref
                  fun null(q: 'q queue ref) =
                     case !q of
                        Nil => true
                      | _ => false
                  val q: (int * int) queue ref = ref Nil
                  val tail: (int * int) queue ref = ref Nil
                  fun enqueue(y, x, value) =
                     if value < matrix_ref(h, y, x)
                        then (matrix_set(h, y, x, value)
                              ; if not(matrix_ref(marked, y, x))
                                   then (matrix_set(marked, y, x, true)
                                         ; (case !tail of
                                               Nil =>
                                                  (tail := Cons((x, y), ref Nil)
                                                   ; q := !tail)
                                             | Cons(_, cdr) =>
                                                  (cdr := Cons((x, y), ref Nil)
                                                   ; tail := !cdr)))
                                else ())
                     else ()
                  fun dequeue() =
                     case !q of
                        Nil => raise Fail "dequeue"
                      | Cons(p, rest) =>
                         (matrix_set(marked, y p, x p, false)
                          ; q := !rest
                          ; if null q then tail := Nil else ()
                          ; p)
               in doo(height, fn y =>
                     doo(width, fn x =>
                        (matrix_set(h, y, x, m1)
                         ; matrix_set(marked, y, x, false))))
                  ; doo(height, fn y =>
                       doo(width, fn x =>
                          if ef_t(y, x)
                             andalso matrix_ref(h, y, x) > 1
                             then enqueue(y, x, 1)
                          else ()))
                  ; let
                       fun loop() =
                          if not(null q)
                             then
                                (let val p = dequeue()
                                     val x = x p
                                     val y = y p
                                     val value = matrix_ref(h, y, x) + 1
                                 in if x > 0 andalso ef_right(y, x - 1)
                                       then enqueue(y, x - 1, value)
                                    else ()
                                    ; if x < width - 1 andalso ef_left(y, x + 1)
                                         then enqueue(y, x + 1, value)
                                      else ()
                                    ; if y > 0 andalso ef_down(y - 1, x)
                                         then enqueue(y - 1, x, value)
                                      else ()
                                    ; if y < height - 1 andalso ef_up(y + 1, x)
                                         then enqueue(y + 1, x, value)
                                      else ()
                                 end
                                 ; loop())
                          else ()
                    in loop()
                    end
               end (* relabel *)
         in doo(height, fn y =>
               doo(width, fn x =>
                  (matrix_set(e, y, x, 0)
                   ; matrix_set(f_t, y, x, 0))))
            ; doo(height, fn y =>
                 doo(width - 1, fn x =>
                    matrix_set(f_right, y, x, 0)))
            ; doo(height - 1, fn y =>
                 doo(width, fn x =>
                    matrix_set(f_down, y, x, 0)))
            ; doo(height, fn y =>
                 (matrix_set(e, y, width - 1, ~1)
                  ; matrix_set(e, y, 0, ~1)))
            ; doo(width - 1, fn x =>
                 (matrix_set(e, height - 1, x, ~1)
                  ; matrix_set(e, 0, x, ~1)))
            ; let val pushes = ref 0
                  val lifts = ref 0
                  val relabels = ref 0
                  fun loop(i, p) =
                     if zero(modulo(i, 6)) andalso not p
                        then (relabel()
                              ; relabels := !relabels + 1
                              ; if every_n(height, fn y =>
                                           every_n(width, fn x =>
                                                   zero(matrix_ref(e, y, x))
                                                   orelse
                                                   matrix_ref(h, y, x) = m1))
                                   then
                                      (* Every vertex with excess capacity is not reachable from the sink in
                                       * the inverse residual network. So terminate early because we have
                                       * already found a min cut. In this case, the preflows and excess
                                       * capacities will not be correct. But the cut is indicated by the
                                       * heights. Vertices reachable from the source have height
                                       * HEIGHT * WIDTH + 2 while vertices reachable from the sink have
                                       * smaller height. Early termination is necessary with relabeling to
                                       * prevent an infinite loop. The loop arises because vertices that are
                                       * not reachable from the sink in the inverse residual network have
                                       * their height reset to HEIGHT * WIDTH + 2 by the relabeling
                                       * process. If there are such vertices with excess capacity, this is
                                       * not high enough for the excess capacity to be pushed back to the
                                       * perimeter. So after relabeling, vertices get lifted to try to push
                                      * excess capacity back to the perimeter but then a relabeling happens
                                      * to soon and foils this lifting. Terminating when all vertices with
                                      * excess capacity are not reachable from the sink in the inverse
                                      * residual network eliminates this problem.
                                      *)
                                      (format
                                       ("~s push~a, ~s lift~a, ~s relabel~a, ~s wave~a, terminated early~%",
                                        [Int(! pushes),
                                         String(if !pushes = 1 then "" else "es"),
                                         Int(! lifts),
                                         String(if !lifts = 1 then "" else "s"),
                                         Int(! relabels),
                                         String(if !relabels = 1 then "" else "s"),
                                         Int i,
                                         String(if i = 1 then "" else "s")]))
                                else
                                   (* We need to rebuild the priority queue after relabeling since the
                                    * heights might have changed and the priority queue is indexed by
                                    * height. This also assumes that a relabel is done before any pushes
                                    * or lifts.
                                    *)
                                   (doo(A.length q, fn k =>
                                       A.update(q, k, []))
                                    ; doo(height, fn y =>
                                         doo(width, fn x =>
                                            matrix_set(marked, y, x, false)))
                                    ; doo(height, fn y =>
                                         doo(width, fn x =>
                                            if not(zero(matrix_ref(e, y, x)))
                                               then enqueue(y, x)
                                            else ()))
                                    ; loop(i, true)))
                     else if A.exists (fn ps =>
                                         some(ps, fn p =>
                                              let val x = x p
                                                 val y = y p
                                              in can_push_right(y, x)
                                                 orelse can_push_left(y, x)
                                                 orelse can_push_down(y, x)
                                                 orelse can_push_up(y, x)
                                                 orelse can_push_t(y, x)
                                                 orelse can_lift(y, x)
                                              end)) q
                             then
                                (
                                 let fun loop k =
                                    if not(negative k)
                                       then
                                          (
                                           let val ps = A.sub(q, k)
                                           in A.update(q, k, [])
                                              ; for_each
                                                 (ps, fn p =>
                                                  matrix_set(marked, y p, x p,
                                                             false))
                                              ; for_each
                                                 (ps, fn p =>
                                                  let val x = x p
                                                     val y = y p
                                                  in if can_push_right(y, x)
                                                        then (pushes := !pushes + 1
                                                              ; push_right(y, x))
                                                     else ()
                                                        ; if can_push_left(y, x)
                                                             then (pushes := !pushes + 1
                                                                   ; push_left(y, x))
                                                          else ()
                                                             ; if can_push_down(y, x)
                                                                  then (pushes := !pushes + 1
                                                                        ; push_down(y, x))
                                                               else ()
                                                                  ; if can_push_up(y, x)
                                                                       then (pushes := !pushes + 1
                                                                             ; push_up(y, x))
                                                                    else ()
                                                                       ; if can_push_t(y, x)
                                                                            then (pushes := !pushes + 1
                                                                                  ; push_t(y, x))
                                                                         else ()
                                                                            ; if can_lift(y, x)
                                                                                 then (lifts := !lifts + 1
                                                                                       ; lift(y, x))
                                                                              else ()
                                                                                 ; if not(zero(matrix_ref(e, y, x)))
                                                                                      then enqueue(y, x)
                                                                                   else ()
                                                  end)
                                           end
                                        ; loop(k - 1))
                                    else ()
                                 in loop(A.length q - 1)
                                 end
                              ; loop(i + 1, false))
                          else
                             (* This is so MIN_CUT and MIN_CUT_INCLUDES_EVERY_EDGE_TO_T work. *)
                             (relabel()
                              ; relabels := !relabels + 1
                              ; (format("~s push~a, ~s lift~a, ~s relabel~a, ~s wave~a~%",
                                          [Int(! pushes),
                                           String(if !pushes = 1 then "" else "es"),
                                           Int(! lifts),
                                           String(if !lifts = 1 then "" else "s"),
                                           Int(! relabels),
                                           String(if !relabels = 1 then "" else "s"),
                                           Int i,
                                           String(if i = 1 then "" else "s")])))
              in loop(0, false)
              end
         end
           fun min_cut_includes_every_edge_to_t() =
              (* This requires that a relabel was done immediately before returning from
               * PREFLOW_PUSH.
               *)
             every_n(height, fn y =>
                     every_n(width, fn x =>
                             matrix_ref(h, y, x) = m1))
           fun min_cut() =
              (* This requires that a relabel was done immediately before returning from
               * PREFLOW_PUSH
               *)
              A.tabulate
              (height, fn y =>
               A.tabulate(width, fn x =>
                            not(matrix_ref(h, y, x) = m1)))
           fun loop(lg_v, v_max) =
              if negative lg_v
                 then (format("V-MAX=~s~%",[Int v_max])
                       ; preflow_push(v_max + 1)
                       ; min_cut())
              else let val v = v_max + let
                                          fun loop(i, c) =
                                             if (zero i)
                                                then c
                                             else loop(i - 1, c + c)
                                       in loop(lg_v, 1)
                                       end
                   in format("LG-V=~s, V-MAX=~s, V=~s~%",
                             [Int lg_v, Int v_max, Int v])
                      ; preflow_push v
                      ; loop(lg_v - 1,
                             if min_cut_includes_every_edge_to_t()
                                then v
                             else v_max)
                   end
   in loop(lg_max_v, 0)
   end

    fun run n = let
        val height = n
        val width = n
        val lg_max_v = 15
        val c_right = make_matrix(height, width - 1, ~1)
        val c_down = make_matrix(height - 1, width, ~1)
        in
          doo(height, fn y =>
            doo(width - 1, fn x =>
              matrix_set
              (c_right, y, x,
               if (y >= quotient(height, 4)
                   andalso y < quotient(3 * height, 4)
                   andalso (x = quotient(width, 4) - 1
                            orelse x = quotient(3 * width, 4) - 1))
                  then 1
               else 128)))
        ; doo(height - 1, fn y =>
            doo(width, fn x =>
                matrix_set
                (c_down, y, x,
                 if (x >= quotient(width, 4)
                     andalso x < quotient(3 * width, 4)
                     andalso (y = quotient(height, 4) - 1
                              orelse y = quotient(3 * height, 4) - 1))
                    then 1
                 else 128)))
        ; rao_ratio_region(c_right, c_down,
                         make_matrix(height, width, 1),
                         lg_max_v)
        end

    fun testit () = ignore (run 50)

    fun doit () = ignore (run 500)

  end
end

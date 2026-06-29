(* all.sml -- all sources for tyan *)
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
(******************** ../BASIS/string.sig ********************)
(* string.sig
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

signature STRING =
  sig
    eqtype char
    eqtype string

    val maxSize : int
    val size      : string -> int

    val sub       : string * int -> char

    val str       : char -> string
    val extract   : string * int * int option -> string
    val substring : string * int * int -> string

    val ^         : string * string -> string
    val concat    : string list -> string
    val concatWith : string -> string list -> string

    val implode   : char list -> string
    val explode   : string -> char list
    val map       : (char -> char) -> string -> string
    val translate : (char -> string) -> string -> string
    val tokens    : (char -> bool) -> string -> string list
    val fields    : (char -> bool) -> string -> string list

    val isPrefix    : string -> string -> bool
    val isSubstring : string -> string -> bool
    val isSuffix    : string -> string -> bool

    val compare  : string * string -> order
    val collate  : (char * char -> order) -> string * string -> order

    val <  : (string * string) -> bool
    val <= : (string * string) -> bool
    val >  : (string * string) -> bool
    val >= : (string * string) -> bool

    val toString    : string -> String.string
    val scan        : (char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader
    val fromString  : String.string -> string option
    val toCString   : string -> String.string
    val fromCString : String.string -> string option

    (* Basis Library proposal 2015-003 *)
    val rev           : string -> string
    val implodeRev    : char list -> string

    val concatWithMap : string -> ('a -> string) -> 'a list -> string

  end
(******************** ../BASIS/string.sml ********************)
(* string.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure String : STRING =
  struct

    structure V = Unsafe.CharVector

  (* fast add/subtract avoiding the overflow test *)
    infix -- ++
    fun x -- y = Word.toIntX(Word.fromInt x - Word.fromInt y)
    fun x ++ y = Word.toIntX(Word.fromInt x + Word.fromInt y)

    fun uLessThan (x, y) = Word.<(Word.fromInt x, Word.fromInt y)

    val unsafeSub = V.sub
    val unsafeUpdate = V.update
    val unsafeCreate = V.create

  (* list reverse *)
    fun listRev ([], l) = l
      | listRev (x::r, l) = listRev (r, x::l)

    type char = char
    type string = string

    val maxSize = CharVector.maxLen

  (* these functions are implemented in base/system/smlnj/init/pervasive.sml *)
    val size = CharVector.length
    val op ^ = op ^
    val concat = concat
    val implode = implode
    val explode = explode
    val substring = substring

  (* allocate an uninitialized string of given length *)
    fun create n = if (uLessThan(maxSize, n))
	  then raise General.Size
	  else unsafeCreate n

    val chars = let
          fun next i = if (i <= 255)
                then let
                  val s = unsafeCreate 1
                  in
                    unsafeUpdate(s, 0, chr i);  s :: next(i+1)
                  end
                else []
          in
            Vector.fromList(next 0)
          end

    fun unsafeSubstring (_, _, 0) = ""
      | unsafeSubstring (s, i, 1) =
	  Vector.sub (chars, ord (unsafeSub (s, i)))
      | unsafeSubstring (s, i, n) = let
	  val ss = unsafeCreate n
	  fun copy j = if (j = n)
		then ()
		else (unsafeUpdate(ss, j, unsafeSub(s, i+j)); copy(j+1))
	  in
	    copy 0; ss
	  end

  (* concatenate a pair of non-empty strings *)
    fun concat2 (x, y) = let
	  val xl = size x and yl = size y
	  val ss = create(xl+yl)
	  fun copyx n = if (n = xl)
		then ()
		else (unsafeUpdate(ss, n, unsafeSub(x, n)); copyx(n+1))
	  fun copyy n = if (n = yl)
		then ()
		else (unsafeUpdate(ss, xl+n, unsafeSub(y,n)); copyy(n+1))
	  in
	    copyx 0; copyy 0;
	    ss
	  end

  (* given a reverse order list of strings and a total length, return
   * the concatenation of the list.
   *)
    fun revConcat (0, _) = ""
      | revConcat (1, lst) = let
	  fun find ("" :: r) = find r
	    | find (s :: _) = s
	    | find _ = "" (** impossible **)
	  in
	    find lst
	  end
      | revConcat (totLen, lst) = let
	  val ss = create totLen
	  fun copy ([], _) = ()
	    | copy (s::r, i) = let
		val len = size s
		val i = i - len
		fun copy' j = if (j = len)
		      then ()
		      else (
			unsafeUpdate(ss, i+j, unsafeSub(s, j));
			copy'(j+1))
		in
		  copy' 0;
		  copy (r, i)
		end
	  in
	    copy (lst, totLen);  ss
	  end

  (* added for Basis Library proposal 2015-003 *)
    fun implodeRev [] = ""
      | implodeRev l = let
	  fun length l = let
		fun loop (n, []) = n
		  | loop (n, [_]) = n ++ 1
		  | loop (n, _ :: _ :: l) = loop (n ++ 2, l)
		in
		  loop (0, l)
		end
	  val n = length l
	  val s = create n
	  fun fill ([], _) = s
	    | fill (c::cs, i) = (
		unsafeUpdate(s, i, c);
		fill (cs, i -- 1))
	  in
	    fill (l, n -- 1)
	  end
  (* end Basis Library proposal 2015-003 *)

  (* convert a character into a single character string *)
    fun str (c : Char.char) : string = Vector.sub(chars, ord c)

  (* get a character from a string *)
    val sub : (string * int) -> char = CharVector.sub

    fun extract (v, base, optLen) = let
	  val len = size v
	  fun newVec n = let
		val newV = unsafeCreate n
		fun fill i = if (i < n)
		      then (unsafeUpdate(newV, i, unsafeSub(v, base ++ i)); fill(i ++ 1))
		      else ()
		in
		  fill 0; newV
		end
	  in
	    case (base, optLen)
	     of (0, NONE) => v
	      | (_, SOME 0) => if ((base < 0) orelse (len < base))
		    then raise General.Subscript
		    else ""
	      | (_, NONE) => if ((base < 0) orelse (len < base))
		      then raise General.Subscript
		    else if (base = len)
		      then ""
		      else newVec (len - base)
	      | (_, SOME 1) =>
		  if ((base < 0) orelse (len < (base ++ 1)))
		    then raise General.Subscript
		    else str(unsafeSub(v, base))
	      | (_, SOME n) =>
		  if ((base < 0) orelse (n < 0) orelse (len < (base ++ n)))
		    then raise General.Subscript
		    else newVec n
	    (* end case *)
	  end

  (* concatenate a list of strings, using the given separator string *)
    fun concatWith _ [] = ""
      | concatWith _ [x] = x
      | concatWith sep (h :: t) =
	  concat (listRev (foldl (fn (x, l) => x :: sep :: l) [h] t, []))

    fun map f vec = (case (size vec)
	   of 0 => ""
	    | len => let
		val newVec = unsafeCreate len
		fun mapf i = if (i < len)
		      then (unsafeUpdate(newVec, i, f(unsafeSub(vec, i))); mapf(i+1))
		      else ()
		in
		  mapf 0; newVec
		end
	  (* end case *))

  (* map a translation function across the characters of a string *)
    fun translate tr s = let
	  val stop = size s
	  fun mkList (i, totLen, lst) = if (i < stop)
		then let val s' = tr (unsafeSub (s, i))
		  in
		    mkList (i+1, totLen + size s', s' :: lst)
		  end
		else revConcat (totLen, lst)
          in
	    mkList (0, 0, [])
          end

  (* tokenize a string using the given predicate to define the delimiter
   * characters.
   *)
    fun tokens isDelim s = let
	  val n = size s
	  fun substr (i, j, l) = if (i = j)
		then l
		else unsafeSubstring(s, i, j -- i)::l
	  fun scanTok (i, j, toks) = if (j < n)
		  then if (isDelim (unsafeSub (s, j)))
		    then skipSep(j ++ 1, substr(i, j, toks))
		    else scanTok (i, j ++ 1, toks)
		  else substr(i, j, toks)
	  and skipSep (j, toks) = if (j < n)
		  then if (isDelim (unsafeSub (s, j)))
		    then skipSep(j ++ 1, toks)
		    else scanTok(j, j ++ 1, toks)
		  else toks
	  in
	    listRev (scanTok (0, 0, []), [])
	  end
    fun fields isDelim s = let
	  val n = size s
	  fun substr (i, j, l) = unsafeSubstring(s, i, j -- i)::l
	  fun scanTok (i, j, toks) = if (j < n)
		  then if (isDelim (unsafeSub (s, j)))
		    then scanTok (j+1, j+1, substr(i, j, toks))
		    else scanTok (i, j+1, toks)
		  else substr(i, j, toks)
	  in
	    listRev (scanTok (0, 0, []), [])
	  end

  (* String comparisons *)
    local
      fun isPrefix' (s1, s2, i2, n2) = let
            val n1 = size s1
            fun eq (i, j) =
                  (i >= n1)
                  orelse ((unsafeSub(s1, i) = unsafeSub(s2, j)) andalso eq(i+1, j+1))
            in
              (n2 >= n1) andalso eq (0, i2)
            end
    in
    fun isPrefix s1 s2 = isPrefix' (s1, s2, 0, size s2)
    fun isSuffix s1 s2 = let
          val sz2 = size s2
          in
            isPrefix' (s1, s2, sz2 - size s1, sz2)
          end
    end

    (* Knuth-Morris-Pratt String Matching
     *
     * val kmp : string -> string * int * int -> int option
     * Find the first string within the second, starting at and
     * ending before the given positions.
     * Return the starting position of the match
     * or the given ending position if there is no match. *)
    fun kmp pattern = let
          val psz = size pattern
          val next = Array.array (psz, ~1)
          fun pat x = unsafeSub (pattern, x)
          fun nxt x = Array.sub (next, x)
          fun setnxt (i, x) = Array.update (next, i, x)
          (* trying to fill next at position p (> 0) and higher;
           * invariants: x >= 0
           *             pattern[0..x) = pattern[p-x..p)
           *             for i in [0..p) :
           *                 pattern[i] <> pattern[next[i]]
           *                 pattern[0..next[i]) = pattern[i-next[i]..i) *)
          fun fill (p, x) = if p >= psz then ()
                            else if pat x = pat p then dnxt (p, nxt x, x + 1)
                            else dnxt (p, x, nxt x + 1)
          and dnxt (p, x, y) = (setnxt (p, x); fill (p + 1, y))
          (* Once next has been initialized, it serves the following purpose:
           * Suppose we are looking at text position t and pattern position
           * p.  This means that all pattern positions < p have already
           * matched the text positions that directly precede t.
           * Now, if the text[t] matches pattern[p], then we simply advance
           * both t and p and try again.
           * However, if the two do not match, then we simply
           * try t again, but this time with the pattern position
           * given by next[p].
           * Success is when p reaches the end of the pattern, failure is
           * when t reaches the end of the text without p having reached the
           * end of the pattern. *)
          fun search (text, start, tsz) = let
                fun txt x = unsafeSub (text, x)
                fun loop (p, t) =
                    if p >= psz then t - psz
                    else if t >= tsz then tsz
                    else if p < 0 orelse pat p = txt t then loop (p+1, t+1)
                    else loop (nxt p, t)
                in
                  loop (0, start)
                end
          in
            fill (1, 0); search
          end

    fun isSubstring s = let
          val stringsearch = kmp s
          fun search s' = let
              val epos = size s'
              in
                stringsearch (s', 0, epos) < epos
              end
          in
            search
          end

    fun collate cmpFn (s1, s2) = let
          val n1 = size s1
          val n2 = size s2
	  val (n, order) =
		if (n1 = n2) then (n1, EQUAL)
		else if (n1 < n2) then (n1, LESS)
		else (n2, GREATER)
	  fun cmp i = if (i = n)
		then order
		else let
		  val c1 = unsafeSub(s1, i)
		  val c2 = unsafeSub(s2, i)
		  in
		    case (cmpFn(c1, c2))
		     of EQUAL => cmp (i+1)
		      | order => order
		    (* end case *)
		  end
          in
            cmp 0
          end

    fun compare (a, b) = let
	  fun cmpFn (c1, c2) =
		if (c1 = c2) then EQUAL
		else if (Char.>(c1, c2)) then GREATER
		else LESS
          in
            collate cmpFn (a, b)
          end

    val scan = String.scan
    val fromString = StringCvt.scanString scan
    val toString = translate Char.toString
    val fromCString = String.fromCString
    val toCString = translate Char.toCString

  (* added for Basis Library proposal 2015-003 *)
    fun rev s = let
	  val n = size s
	  in
	    if (n < 2)
	      then s
	      else let
		val s' = unsafeCreate n
		fun fill i = if (i < n)
		      then (unsafeUpdate(s', i, unsafeSub(s, n--i--1)); fill(i++1))
		      else ()
		in
		  fill 0; s'
		end
	  end

    fun concatWithMap sep cvtFn = let
	  fun concat' [] = ""
	    | concat' [x] = cvtFn x
	    | concat' (x::xs) = let
		val sepLen = size sep
		fun cvt ([], strs, len) = let
		      val s' = unsafeCreate len
		      fun fill ([], _) = s'
			| fill (s::ss, i) = let
			    val n = size s
			    val i = i -- n
			    fun copy j = if j < n
				  then (unsafeUpdate(s', i++j, unsafeSub(s, j)); copy(j++1))
				  else fill (ss, i)
			    in
			      copy 0
			    end
		      in
			fill (strs, len)
		      end
		  | cvt (x::xs, strs, len) = let
		      val s = cvtFn x
		      val len = len ++ sepLen ++ size s
		      in
			if len > maxSize then raise General.Size else ();
			cvt (xs, s::sep::strs, len)
		      end
		val s = cvtFn x
		in
		  cvt (xs, [s], size s)
		end
	  in
	    concat'
	  end
  (* end Basis Library proposal 2015-003 *)

  (* String greater or equal *)
    fun sgtr (a, b) = let
	  val al = size a and bl = size b
	  val n = if (al < bl) then al else bl
	  fun cmp i = if (i = n)
		then (al > bl)
		else let
		  val ai = unsafeSub(a,i)
		  val bi = unsafeSub(b,i)
		  in
		    Char.>(ai, bi) orelse ((ai = bi) andalso cmp(i+1))
		  end
	  in
	    cmp 0
	  end

    fun op <= (a,b) = if sgtr(a,b) then false else true
    fun op < (a,b) = sgtr(b,a)
    fun op >= (a,b) = b <= a
    val op > = sgtr

  end

(* pervasive string functions *)
val concat = String.concat
val op ^ = String.^
val str = String.str
val size = String.size
val implode = String.implode
val explode = String.explode
val substring = String.substring
(******************** util.sml ********************)
(* util.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Util = struct
    exception NotImplemented of string
    exception Impossible of string (* flag "impossible" condition  *)
    exception Illegal of string (* flag function use violating precondition *)

    fun error exn msg = raise (exn msg)
    fun notImplemented msg = error NotImplemented msg
    fun impossible msg = error Impossible msg
    fun illegal msg = error Illegal msg

    (* arr[i] := obj :: arr[i]; extend non-empty arr if necessary *)
    fun insert (obj,i,arr) = let
          val len = Array.length arr
          val res =  if i<len then (Array.update(arr,i,obj::Array.sub(arr,i)); arr)
             else let val arr' = Array.array(Int.max(i+1,len+len),[])
                      fun copy ~1 = (Array.update(arr',i,[obj]); arr')
                        | copy j = (Array.update(arr',j,Array.sub(arr,j));
                                    copy(j-1))
                      in copy(len-1) end
          in res
          end

    (* given compare and array a, return list of contents of a sorted in
     * ascending order, with duplicates stripped out; which copy of a duplicate
     * remains is random.  NOTE that a is modified.
     *)
    fun stripSort compare = fn a => let
          infix sub


          val op sub = Array.sub and update = Array.update
          fun swap (i,j) = let val ai = a sub i
                           in update(a,i,a sub j); update(a,j,ai) end
          (* sort all a[k], 0<=i<=k<j<=length a *)
          fun s (i,j,acc) = if i=j then acc else let
                val pivot = a sub ((i+j) div 2)
                fun partition (lo,k,hi) = if k=hi then (lo,hi) else
                      case compare (pivot,a sub k) of
                          LESS => (swap (lo,k); partition (lo+1,k+1,hi))
                        | EQUAL => partition (lo,k+1,hi)
                        | GREATER => (swap (k,hi-1); partition (lo,k,hi-1))
                val (lo,hi) = partition (i,i,j)
                in s(i,lo,pivot::s(hi,j,acc)) end
           val res = s(0,Array.length a,[])

          in
           res
          end
end
(******************** f.sml ********************)
(* f.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure F = struct

    val p = 17

    datatype field = F of int (* for (F n), always 0<=n<p *)

    (* exception Div = Integer.Div *)
(* unused code unless P.show, commented out in earlier version, is used
    fun show (F x) = Log.print (Int.toString x)
*)
(* unused code
    val char = p
*)

(* unused code unless P.display is used
    val zero = F 0
*)
    val one = F 1
    fun coerceInt n = F (n mod p)

    fun add (F n,F m) = let val k = n+m in if k>=p then F(k-p) else F k end
    fun subtract (F n,F m) = if n>=m then F(n-m) else F(n-m+p)
    fun negate (F 0) = F 0 | negate (F n) = F(p-n)
    fun multiply (F n,F m) = F ((n*m) mod p)
    fun reciprocal (F 0) = raise Div
      | reciprocal (F n) = let
          (* consider euclid gcd alg on (a,b) starting with a=p, b=n.
           * if maintain a = a1 n + a2 p, b = b1 n + b2 p, a>b,
           * then when 1 = a = a1 n + a2 p, have a1 = inverse of n mod p
           * note that it is not necessary to keep a2, b2 around.
           *)
          fun gcd ((a,a1),(b,b1)) =
              if b=1 then (* by continued fraction expansion, 0<|b1|<p *)
                 if b1<0 then F(p+b1) else F b1
              else let val q = a div b
                   in gcd((b,b1),(a-q*b,a1-q*b1)) end
          in gcd ((p,0),(n,1)) end
(* unused code
    fun divide (n,m) = multiply (n, reciprocal m)
*)

(* unused code unless power is used
    val andb = op &&
    val rshift = op >>
*)

(* unused code
    fun power(n,k) =
          if k<=3 then case k of
              0 => one
            | 1 => n
            | 2 => multiply(n,n)
            | 3 => multiply(n,multiply(n,n))
            | _ => reciprocal (power (n,~k)) (* know k<0 *)
          else if andb(k,1)=0 then power(multiply(n,n),rshift(k,1))
               else multiply(n,power(multiply(n,n),rshift(k,1)))
*)

    fun isZero (F n) = n=0
(* unused codeunless P.display is used
    fun equal (F n,F m) = n=m

    fun display (F n) = if n<=p div 2 then Int.toString n
                        else "-" ^ Int.toString (p-n)
*)
end
(******************** m.sml ********************)
(* m.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure M = struct (* MONO *)
    local
        val andb = fn (i1, i2) => (Word.toInt (Word.andb (Word.fromInt (i1), Word.fromInt (i2))))
        val op << = fn (i1, i2) => (Word.toInt (Word.<< (Word.fromInt (i1), Word.fromInt (i2))))
        val op >> = fn (i1, i2) => (Word.toInt (Word.>> (Word.fromInt (i1), Word.fromInt (i2))))
        infix << >> andb
    in

(* encode (var,pwr) as a long word: hi word is var, lo word is pwr
   masks 0xffff for pwr, mask ~0x10000 for var, rshift 16 for var
   note that encoded pairs u, v have same var if u>=v, u andb ~0x10000<v
*)

    datatype mono = M of int list
    exception DoesntDivide

    val one = M []
    fun x_i v = M [(v<<16)+1]
    fun explode (M l) = List.map (fn v => (v>>16,v andb 65535)) l
    fun implode l = M (List.map (fn (v,p) => (v<<16)+p) l)

    val deg = let fun d([],n) = n | d(u::ul,n) = d(ul,(u andb 65535) + n)
              in fn (M l) => d(l,0) end

    (* x^k > y^l if x>k or x=y and k>l *)
    val compare = let
          fun cmp ([],[]) = EQUAL
            | cmp (_::_,[]) = GREATER
            | cmp ([],_::_) = LESS
            | cmp ((u::us), (v::vs)) = if u=v then cmp (us,vs)
                                  else if u<v then LESS
                                  else (* u>v *)   GREATER
          in fn (M m,M m') => cmp(m,m') end

    fun display (M (l : int list)) : string =
      let
        fun dv v = if v<26 then chr (v+ord #"a") else chr (v-26+ord #"A")
        fun d (vv,acc) = let val v = vv>>16 and p = vv andb 65535
                         in if p=1 then dv v::acc
                            else
                              (dv v)::(String.explode (Int.toString p)) @ acc
                         end
      in String.implode(List.foldl d [] l) end

    val multiply = let
          fun mul ([],m) = m
            | mul (m,[]) = m
            | mul (u::us, v::vs) = let
                val uu = u andb ~65536
                in if uu = (v andb ~65536) then let
                      val w = u + (v andb 65535)
                      in if uu = (w andb ~65536) then w::mul(us,vs)
                         else
                           (Util.illegal
                            (String.concat ["Mono.multiply overflow: ",
                                            display (M(u::us)),", ",
                                            display (M(v::vs))]))
                      end
                   else if u>v then u :: mul(us,v::vs)
                   else (* u<v *) v :: mul(u::us,vs)
                end
          in fn (M m,M m') => M (mul (m,m')) end

    val lcm = let
          fun lcm ([],m) = m
            | lcm (m,[]) = m
            | lcm (u::us, v::vs) =
                if u>=v then if (u andb ~65536)<v then u::lcm(us,vs)
                                                    else u::lcm(us,v::vs)
                        else if (v andb ~65536)<u then v::lcm(us,vs)
                                                    else v::lcm(u::us,vs)
          in fn (M m,M m') => M (lcm (m,m')) end
    val tryDivide = let
          fun rev([],l) = l | rev(x::xs,l)=rev(xs,x::l)
          fun d (m,[],q) = SOME(M(rev(q,m)))
            | d ([],_::_,_) = NONE
            | d (u::us,v::vs,q) =
                if u<v then NONE
                else if (u andb ~65536) = (v andb ~65536) then
                    if u=v then d(us,vs,q) else d(us,vs,u-(v andb 65535)::q)
                else d(us,v::vs,u::q)
          in fn (M m,M m') => d (m,m',[]) end
    fun divide (m,m') =
          case tryDivide(m,m') of SOME q => q | NONE => raise DoesntDivide

end end (* local, structure M *)
(******************** mi.sml ********************)
(* mi.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure MI = struct (* MONO_IDEAL *)

    (* trie:
     * index first by increasing order of vars
     * children listed in increasing degree order
     *)
    datatype 'a mono_trie = MT of 'a option * (int * 'a mono_trie) list
                            (* tag, encoded (var,pwr) and children *)
    datatype 'a mono_ideal = MI of (int * 'a mono_trie) ref
                            (* int maxDegree = least degree > all elements *)

    val op && = fn (i1, i2) => (Word.toInt (Word.andb (Word.fromInt (i1), Word.fromInt (i2))))
    val op << = fn (i1, i2) => (Word.toInt (Word.<< (Word.fromInt (i1), Word.fromInt (i2))))
    infix && <<

    fun rev ([],l) = l | rev (x::xs,l) = rev(xs,x::l)
    val emptyTrie = MT(NONE,[])
    fun mkEmpty () = MI(ref (0,emptyTrie))

    val lshift = op <<
    val andb = op &&

    fun encode (var,pwr) = lshift(var,16)+pwr
    fun grabVar vp = andb(vp,~65536)
    fun grabPwr vp = andb(vp,65535)
    fun smallerVar (vp,vp') = vp < andb(vp',~65536)

    exception Found
    fun search (MI(x),M.M m') = let
          val (d,mt) = !x
          val result = ref NONE
          (* exception Found of M.mono * '_a *)
          (* s works on remaining input mono, current output mono, tag, trie *)
          fun s (_,m,MT(SOME a,_)) =
                raise(result := SOME (M.M m,a); Found)
            | s (m',m,MT(NONE,trie)) = s'(m',m,trie)
          and s'([],_,_) = NONE
            | s'(_,_,[]) = NONE
            | s'(vp'::m',m,trie as (vp,child)::children) =
                if smallerVar(vp',vp) then s'(m',m,trie)
                else if grabPwr vp = 0 then (s(vp'::m',m,child);
                                             s'(vp'::m',m,children))
                else if smallerVar(vp,vp') then NONE
                else if vp<=vp' then (s(m',vp::m,child);
                                      s'(vp'::m',m,children))
                else NONE
          in s(rev(m',[]),[],mt)
             handle Found (* (m,a) => SOME(m,a) *) => !result
          end

   (* assume m is a new generator, i.e. not a multiple of an existing one *)
    fun insert (MI (mi),m,a) = let
          val (d,mt) = !mi
          fun i ([],MT (SOME _,_)) = Util.illegal "MONO_IDEAL.insert duplicate"
            | i ([],MT (NONE,children)) = MT(SOME a,children)
            | i (vp::m,MT(a',[])) = MT(a',[(vp,i(m,emptyTrie))])
            | i (vp::m,mt as MT(a',trie as (vp',_)::_)) = let
                fun j [] = [(vp,i(m,emptyTrie))]
                  | j ((vp',child)::children) =
                      if vp<vp' then (vp,i(m,emptyTrie))::(vp',child)::children
                      else if vp=vp' then (vp',i(m,child))::children
                      else (vp',child) :: j children
                in
                   if smallerVar(vp,vp') then
                       MT(a',[(grabVar vp,MT(NONE,trie)),(vp,i(m,emptyTrie))])
                   else if smallerVar(vp',vp) then i(grabVar vp'::vp::m,mt)
                   else MT(a',j trie)
                end
          in mi := (Int.max(d,M.deg m),i (rev(List.map encode(M.explode m),[]),mt)) end

    fun mkIdeal [] = mkEmpty()
      | mkIdeal (orig_ms : (M.mono * '_a) list)= let
          fun ins ((m,a),arr) = Util.insert((m,a),M.deg m,arr)
          val msa = Array.fromList orig_ms
          val ms : (M.mono * '_a) list =
              Util.stripSort (fn ((m,_),(m',_)) => M.compare (m,m')) msa
          val buckets = List.foldr ins (Array.array(0,[])) ms
          val n = Array.length buckets
          val mi = mkEmpty()
          fun sort i = if i>=n then mi else let
                fun redundant (m,_) = case search(mi,m) of NONE => false
                                                         | SOME _ => true
                fun filter ([],l) = List.app (fn (m,a) => insert(mi,m,a)) l
                  | filter (x::xx,l) = if redundant x then filter(xx,l)
                                       else filter(xx,x::l)
                in filter(Array.sub(buckets,i),[]);
                   Array.update(buckets,i,[]);
                   sort(i+1)
                end
          in sort 0 end

    fun fold g (MI(x)) init = let
          val (_,mt) = !x
          fun f(acc,m,MT(NONE,children)) = f'(acc,m,children)
            | f(acc,m,MT(SOME a,children)) =
                f'(g((M.M m,a),acc),m,children)
          and f'(acc,m,[]) = acc
            | f'(acc,m,(vp,child)::children) =
                if grabPwr vp=0 then f'(f(acc,m,child),m,children)
                else f'(f(acc,vp::m,child),m,children)
          in f(init,[],mt) end

end (* structure MI *)
(******************** p.sml ********************)
(* p.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure P = struct

    datatype poly = P of (F.field*M.mono) list (* descending mono order *)
    val zero = P []
    fun coerce (a,m) = P [(a,m)]
    fun implode p = P p
    fun cons (am,P p) = P (am::p)

    val op >> = fn (i1, i2) => (Word.toInt (Word.>> (Word.fromInt (i1), Word.fromInt (i2))))
    infix >>

    val log = let fun log(n,l) = if n<=1 then l else log((n >> 1),1+l)
              in fn n => log(n,0) end
    val maxLeft = ref 0
    val maxRight = ref 0
    val counts = Array.tabulate(20,fn _ => Array.array(20,0))
    val indices = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]

    fun pair(l,r) = let
          val l = log l and r = log r
          val _ = maxLeft := Int.max(!maxLeft,l) and _ = maxRight := Int.max(!maxRight,r)
          val a = Array.sub(counts,l)
          in Array.update(a,r,Array.sub(a,r)+1) end

local
    fun neg p = (List.map (fn (a,m) => (F.negate a,m)) p)
    fun plus ([],p2) = p2
      | plus (p1,[]) = p1
      | plus ((a,m)::ms,(b,n)::ns) = case M.compare(m,n) of
            LESS => (b,n) :: plus ((a,m)::ms,ns)
          | GREATER => (a,m) :: plus (ms,(b,n)::ns)
          | EQUAL => let val c = F.add(a,b)
                             in if F.isZero c then plus(ms,ns)
                                else (c,m)::plus(ms,ns)
                             end
    fun minus ([],p2) = neg p2
      | minus (p1,[]) = p1
      | minus ((a,m)::ms,(b,n)::ns) = case M.compare(m,n) of
            LESS => (F.negate b,n) :: minus ((a,m)::ms,ns)
          | GREATER => (a,m) :: minus (ms,(b,n)::ns)
          | EQUAL => let val c = F.subtract(a,b)
                             in if F.isZero c then minus(ms,ns)
                                else (c,m)::minus(ms,ns)
                             end
    fun termMult (a,m,p) =
          (List.map (fn (a',m') => (F.multiply(a,a'),M.multiply(m,m'))) p)
in
    fun add (P p1,P p2) = (pair(length p1,length p2); P (plus(p1,p2)))
    fun subtract (P p1,P p2) = (pair(length p1,length p2); P (minus(p1,p2)))

    fun spair (a,m,P f,b,n,P g) =
      (pair(length f,length g); P(minus(termMult(a,m,f),termMult(b,n,g))))
    val termMult = fn (a,m,P f) => P(termMult(a,m,f))
end

    fun scalarMult (a,P p) = P (List.map (fn (b,m) => (F.multiply(a,b),m)) p)

    fun isZero (P []) = true | isZero (P (_::_)) = false

    (* these should only be called if there is a leading term, i.e. poly<>0 *)
    fun leadMono (P((_,m)::_)) = m
      | leadMono (P []) = Util.illegal "POLY.leadMono"
    fun leadCoeff (P((a,_)::_)) = a
      | leadCoeff (P []) = Util.illegal "POLY.leadCoeff"
    fun rest (P (_::p)) = P p
      | rest (P []) = Util.illegal "POLY.rest"
    fun leadAndRest (P (lead::rest)) = (lead,P rest)
      | leadAndRest (P []) = Util.illegal "POLY.leadAndRest"

    fun deg (P []) = Util.illegal "POLY.deg on zero poly"
      | deg (P ((_,m)::_)) = M.deg m (* homogeneous poly *)
    fun numTerms (P p) = length p

end

(******************** hp.sml ********************)
(* hp.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure HP = struct
    datatype hpoly = HP of P.poly array
    val op >> = fn (i1, i2) => (Word.toInt (Word.>> (Word.fromInt (i1), Word.fromInt (i2))))
    infix >>
    val log = let
          fun log(n,l) = if n<8 then l else log((n >> 2),1+l)
          in fn n => log(n,0) end
    fun mkHPoly p = let
          val l = log(P.numTerms p)
          in HP(Array.tabulate(l+1,fn i => if i=l then p else P.zero)) end
    fun add(p,HP ps) = let
          val l = log(P.numTerms p)
          in if l>=Array.length ps then let
               val n = Array.length ps
               in HP(Array.tabulate(n+n,
                     fn i => if i<n then Array.sub(ps,i)
                             else if i=l then p else P.zero))
               end
             else let
               val p = P.add(p,Array.sub(ps,l))
               in if l=log(P.numTerms p) then (Array.update(ps,l,p); HP ps)
                  else (Array.update(ps,l,P.zero); add (p,HP ps))
               end
          end
    fun leadAndRest (HP ps) = let
          val n = Array.length ps
          fun lar (m,indices,i) = if i>=n then lar'(m,indices) else let
                val p = Array.sub(ps,i)
                in if P.isZero p then lar(m,indices,i+1)
                   else if null indices then lar(P.leadMono p,[i],i+1)
                        else case M.compare(m,P.leadMono p) of
                            LESS => lar(P.leadMono p,[i],i+1)
                          | EQUAL => lar(m,i::indices,i+1)
                          | GREATER => lar(m,indices,i+1)
                end
          and lar' (_,[]) = NONE
            | lar' (m,i::is) = let
                fun extract i = case P.leadAndRest(Array.sub(ps,i)) of
                      ((a,_),rest) => (Array.update(ps,i,rest); a)
                val a = List.foldr (fn (j,b) => F.add(extract j,b)) (extract i) is
                in if F.isZero a then lar(M.one,[],0) else SOME(a,m,HP ps)
                end
          in lar(M.one,[],0) end
  end
(******************** g.sml ********************)
(* g.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure G = struct
    val autoReduce = ref true
    val maxDeg = ref 10000
    val maybePairs = ref 0
    val primePairs = ref 0
    val usedPairs = ref 0
    val newGens = ref 0

    val op && = fn (i1, i2) => (Word.toInt (Word.andb (Word.fromInt (i1), Word.fromInt (i2))))
    infix &&

    fun reset () = (maybePairs:=0; primePairs:=0; usedPairs:=0; newGens:=0)

    fun inc r = r := !r + 1

    fun reduce (f,mi) = if P.isZero f then f else let
          (* use accumulator and reverse at end? *)
          fun r hp = case HP.leadAndRest hp of
                NONE => []
              | (SOME(a,m,hp)) => case MI.search(mi,m) of
                    NONE => (a,m)::(r hp)
                  | SOME (m',p) => r (HP.add(P.termMult(F.negate a,M.divide(m,m'),!p),hp))
          in P.implode(r (HP.mkHPoly f)) end

    (* assume f<>0 *)
    fun mkMonic f = P.scalarMult(F.reciprocal(P.leadCoeff f),f)

    (* given monic h, a monomial ideal mi of m's tagged with g's representing
     * an ideal (g1,...,gn): a poly g is represented as (lead mono m,rest of g).
     * update pairs to include new s-pairs induced by h on g's:
     * 1) compute minimal gi1...gik so that <gij:h's> generate <gi:h's>, i.e.
     *    compute monomial ideal for gi:h's tagged with gi
     * 2) toss out gij's whose lead mono is rel. prime to h's lead mono (why?)
     * 3) put (h,gij) pairs into degree buckets: for h,gij with lead mono's m,m'
     *    deg(h,gij) = deg lcm(m,m') = deg (lcm/m) + deg m = deg (m':m) + deg m
     * 4) store list of pairs (h,g1),...,(h,gn) as vector (h,g1,...,gn)
     *)
    fun addPairs (h,mi,pairs) = let
          val m = P.leadMono h
          val d = M.deg m
          fun tag ((m' : M.mono,g' : P.poly ref),quots) = (inc maybePairs;
                                     (M.divide(M.lcm(m,m'),m),(m',!g'))::quots)
          fun insert ((mm,(m',g')),arr) = (* recall mm = m':m *)
                if M.compare(m',mm)=EQUAL then (* rel. prime *)
                    (inc primePairs; arr)
                else (inc usedPairs;
                      Util.insert(P.cons((F.one,m'),g'),M.deg mm+d,arr))
          val buckets = MI.fold insert (MI.mkIdeal (MI.fold tag mi []))
                                       (Array.array(0,[]))
          fun ins (~1,pairs) = pairs
            | ins (i,pairs) = case Array.sub(buckets,i) of
                    [] => ins(i-1,pairs)
                  | gs => ins(i-1,Util.insert(Array.fromList(h::gs),i,pairs))
          in ins(Array.length buckets - 1,pairs) end

    fun grobner fs = let
          fun pr l = Log.say (l@["\n"])
          val fs = List.foldr
                (fn (f,fs) => Util.insert(f,P.deg f,fs))
                (Array.array(0,[])) fs
          (* pairs at least as long as fs, so done when done w/ all pairs *)
          val pairs = ref(Array.array(Array.length fs,[]))
          val mi = MI.mkEmpty()
          val newDegGens = ref []
          val addGen = (* add and maybe auto-reduce new monic generator h *)
                if not(!autoReduce) then
                    fn h => MI.insert (mi,P.leadMono h,ref (P.rest h))
                else fn h => let
                    val ((_,m),rh) = P.leadAndRest h
                    fun autoReduce f =
                          if P.isZero f then f
                          else let val ((a,m'),rf) = P.leadAndRest f
                               in case M.compare(m,m') of
                                   LESS => P.cons((a,m'),autoReduce rf)
                                 | EQUAL => P.subtract(rf,P.scalarMult(a,rh))
                                 | GREATER => f
                               end
                    val rrh = ref rh
                    in
                        MI.insert (mi,P.leadMono h,rrh);
                        List.app (fn f => f:=autoReduce(!f)) (!newDegGens);
                        newDegGens := rrh :: !newDegGens
                    end
          val tasksleft = ref 0
          fun feedback () = let
                val n = !tasksleft
                in
                    if (n && 15)=0 then Log.print (Int.toString n) else ();
                        Log.print ".";
                        Log.flush();
                        tasksleft := n-1
                end

          fun try h =
              let
                  val _ = feedback ()
                  val h = reduce(h,mi)
              in if P.isZero h
                     then ()
                 else let val h = mkMonic h
                          val _ = (Log.print "#"; Log.flush())
                      in pairs := addPairs(h,mi,!pairs);
                          addGen h;
                          inc newGens
                      end
              end

          fun tryPairs fgs = let
                val ((a,m),f) = P.leadAndRest (Array.sub(fgs,0))
                fun tryPair i = if i=0 then () else let
                      val ((b,n),g) = P.leadAndRest (Array.sub(fgs,i))
                      val k = M.lcm(m,n)
                      in
                         try (P.spair(b,M.divide(k,m),f,a,M.divide(k,n),g));
                         tryPair (i-1)
                      end
                in tryPair (Array.length fgs -1) end

          fun numPairs ([],n) = n
            | numPairs (p::ps,n) = numPairs(ps,n-1+Array.length p)

          fun gb d = if d>=Array.length(!pairs) then mi else
                (* note: i nullify entries to reclaim space *)
                (
pr ["DEGREE ",Int.toString d," with ",
    Int.toString(numPairs(Array.sub(!pairs,d),0))," pairs ",
    if d>=Array.length fs then "0" else Int.toString(length(Array.sub(fs,d))),
      " generators to do"];
                 tasksleft := numPairs(Array.sub(!pairs,d),0);
                 if d>=Array.length fs then ()
                 else tasksleft := !tasksleft + length (Array.sub(fs,d));
                   if d>(!maxDeg) then ()
                   else (
                         reset();
                         newDegGens := [];
                         List.app tryPairs (Array.sub(!pairs,d));
                         Array.update(!pairs,d,[]);
                         if d>=Array.length fs then ()
                         else (List.app try (Array.sub(fs,d)); Array.update(fs,d,[]));
                           pr ["maybe ",Int.toString(!maybePairs)," prime ",
                               Int.toString (!primePairs),
                               " using ",Int.toString (!usedPairs),
                               "; found ",Int.toString (!newGens)]
                           );
                 gb(d+1)
                )
          in gb 0 end

local
    (* grammar:
     dig  ::= 0 | ... | 9
     var  ::= a | ... | z | A | ... | Z
     sign ::= + | -
     nat  ::= dig | nat dig
     mono ::=  | var mono | var num mono
     term ::= nat mono | mono
     poly ::= term | sign term | poly sign term
    *)
    datatype char = Dig of int | Var of int | Sign of int
    fun char ch =
        let val och = ord ch in
          if ord #"0"<=och andalso och<=ord #"9" then Dig (och - ord #"0")
          else if ord #"a"<=och andalso och<=ord #"z" then Var (och - ord #"a")
          else if ord #"A"<=och andalso och<=ord #"Z" then Var (och - ord #"A" + 26)
          else if och = ord #"+" then Sign 1
          else if och = ord #"-" then Sign ~1
               else Util.illegal ("bad ch in poly: " ^ (Char.toString(ch)))
        end

    fun nat (n,Dig d::l) = nat(n*10+d,l) | nat (n,l) = (n,l)
    fun mono (m,Var v::Dig d::l) =
          let val (n,l) = nat(d,l)
          in mono(M.multiply(M.implode[(v,n)],m),l) end
      | mono (m,Var v::l) = mono(M.multiply(M.x_i v,m),l)
      | mono (m,l) = (m,l)

    fun term l = let
          val (n,l) = case l of (Dig d::l) => nat(d,l) | _ => (1,l)
          val (m,l) = mono(M.one,l)
          in ((F.coerceInt n,m),l) end
    fun poly (p,[]) = p
      | poly (p,l) = let
          val (s,l) = case l of Sign s::l => (F.coerceInt s,l) | _ => (F.one,l)
          val ((a,m),l) = term l
          in poly(P.add(P.coerce(F.multiply(s,a),m),p),l) end

in
    fun parsePoly s = poly (P.zero,List.map char(String.explode s))

end (* local *)

end (* structure G *)
in
(******************** main.sml ********************)
(* main.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "tyan"

    val results = []

    val _ = G.maxDeg:=1000000

    fun grab mi = MI.fold (fn ((m,g),l) => P.cons((F.one,m),!g)::l) mi []

    fun gb fs = let
          val g = G.grobner fs handle (Util.Illegal s) => (Log.print s; raise Div)
          val fs = grab g
          fun info f = Log.say [
                  M.display(P.leadMono f), " + ",
                  Int.toString(P.numTerms f - 1), " terms\n"
                ]
          in
            List.app info fs
          end

    fun doit () = let
          val u6 = List.map G.parsePoly [
                  "abcdef-g6","a+b+c+d+e+f","ab+bc+cd+de+ef+fa",
                  "abc+bcd+cde+def+efa+fab",
                  "abcd+bcde+cdef+defa+efab+fabc",
                  "abcde+bcdef+cdefa+defab+efabc+fabcd"
                ]
          fun loop 0 = ()
            | loop n = (gb u6; loop (n - 1))
           in
             loop 192
           end

    fun testit () = ()

  end
end

(* all.sml -- all sources for life *)
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
(******************** main.sml ********************)
(* life.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "life"

    val results : string list = []

(* TODO: move the list functions to common/list.sml *)
    fun map f [] = []
      | map f (a::x) = f a :: map f x

    fun revAppend ([], l) = l
      | revAppend (x::r, l) = revAppend(r, x::l)

    fun rev l = revAppend(l, [])

    exception ex_undefined of string
    fun error str = raise ex_undefined str

    fun accumulate f = let
	  fun foldf a [] = a
            | foldf a (b::x) = foldf (f a b) x
          in
	    foldf
	  end

    fun filter p = let
	  fun consifp x a = if p a then a::x else x
          in
	    rev o accumulate consifp []
	  end

    fun exists p = let fun existsp [] = false
                     | existsp (a::x) = if p a then true else existsp x
                in existsp end

    fun equal a b = (a  = b)

    fun member x a = exists (equal a) x

    fun C f x y = f y x

    fun cons a x = a::x

    fun revonto x = accumulate (C cons) x

    fun length x = let fun count n a = n+1 in accumulate count 0 x end

    fun repeat f = let fun rptf n x = if n=0 then x else rptf(n-1)(f x)
                       fun check n = if n<0 then error "repeat<0" else n
                    in rptf o check end

    fun copy n x = repeat (cons x) n []

    fun spaces n = concat (copy n " ")

    local
      fun lexordset [] = []
        | lexordset (a::x) = lexordset (filter (lexless a) x) @ [a] @
                             lexordset (filter (lexgreater a) x)
      and lexless(a1:int,b1:int)(a2,b2) =
           if a2<a1 then true else if a2=a1 then b2<b1 else false
      and lexgreater pr1 pr2 = lexless pr2 pr1
      fun collect f list =
             let fun accumf sofar [] = sofar
                   | accumf sofar (a::x) = accumf (revonto sofar (f a)) x
              in accumf [] list
             end
      fun occurs3 x =
          (* finds coords which occur exactly 3 times in coordlist x *)
          let fun f xover x3 x2 x1 [] = diff x3 xover
                | f xover x3 x2 x1 (a::x) =
                   if member xover a then f xover x3 x2 x1 x else
                   if member x3 a then f (a::xover) x3 x2 x1 x else
                   if member x2 a then f xover (a::x3) x2 x1 x else
                   if member x1 a then f xover x3 (a::x2) x1 x else
		                       f xover x3 x2 (a::x1) x
              and diff x y = filter (not o member y) x
           in f [] [] [] [] x end
     in
      abstype generation = GEN of (int*int) list
        with
          fun alive (GEN livecoords) = livecoords
          and mkgen coordlist = GEN (lexordset coordlist)
          and mk_nextgen_fn neighbours gen =
              let val living = alive gen
                  val isalive = member living
                  val liveneighbours = length o filter isalive o neighbours
                  fun twoorthree n = n=2 orelse n=3
	          val survivors = filter (twoorthree o liveneighbours) living
	          val newnbrlist = collect (filter (not o isalive) o neighbours) living
	          val newborn = occurs3 newnbrlist
	       in mkgen (survivors @ newborn) end
     end
    end

    fun neighbours (i,j) = [(i-1,j-1),(i-1,j),(i-1,j+1),
			    (i,j-1),(i,j+1),
			    (i+1,j-1),(i+1,j),(i+1,j+1)]

    local val xstart = 0 and ystart = 0
          fun markafter n string = string ^ spaces n ^ "0"
          fun plotfrom (x,y) (* current position *)
                       str   (* current line being prepared -- a string *)
                       ((x1,y1)::more)  (* coordinates to be plotted *)
              = if x=x1
                 then (* same line so extend str and continue from y1+1 *)
                      plotfrom(x,y1+1)(markafter(y1-y)str)more
                 else (* flush current line and start a new line *)
                      str :: plotfrom(x+1,ystart)""((x1,y1)::more)
            | plotfrom (x,y) str [] = [str]
           fun good (x,y) = x>=xstart andalso y>=ystart
     in  fun plot coordlist = plotfrom(xstart,ystart) ""
                                 (filter good coordlist)
    end


    infix 6 at
    fun coordlist at (x:int,y:int) = let fun move(a,b) = (a+x,b+y)
                                      in map move coordlist end
    val rotate = map (fn (x:int,y:int) => (y,~x))

    val glider = [(0,0),(0,2),(1,1),(1,2),(2,1)]
    val bail = [(0,0),(0,1),(1,0),(1,1)]
    fun barberpole n =
       let fun f i = if i=n then (n+n-1,n+n)::(n+n,n+n)::nil
                       else (i+i,i+i+1)::(i+i+2,i+i+1)::f(i+1)
        in (0,0)::(1,0):: f 0
       end

    val genB = mkgen(glider at (2,2) @ bail at (2,12)
		     @ rotate (barberpole 4) at (5,20))

    fun nthgen g 0 = g | nthgen g i = nthgen (mk_nextgen_fn neighbours g) (i-1)

    val gun = mkgen
     [(2,20),(3,19),(3,21),(4,18),(4,22),(4,23),(4,32),(5,7),(5,8),(5,18),
      (5,22),(5,23),(5,29),(5,30),(5,31),(5,32),(5,36),(6,7),(6,8),(6,18),
      (6,22),(6,23),(6,28),(6,29),(6,30),(6,31),(6,36),(7,19),(7,21),(7,28),
      (7,31),(7,40),(7,41),(8,20),(8,28),(8,29),(8,30),(8,31),(8,40),(8,41),
      (9,29),(9,30),(9,31),(9,32)]

    fun show pr = (app (fn s => (pr s; pr "\n"))) o plot o alive

    fun runOnce () = nthgen gun 50

    fun doit () = let
          fun lp 0 = ()
            | lp n = (runOnce(); lp(n-1))
          in
            lp 1000
          end

    fun testit () = show Log.print (runOnce())

  end (* Main *)
in
(******************** main.sml ********************)
(* life.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : BMARK =
  struct

    val name = "life"

    val results : string list = []

(* TODO: move the list functions to common/list.sml *)
    fun map f [] = []
      | map f (a::x) = f a :: map f x

    fun revAppend ([], l) = l
      | revAppend (x::r, l) = revAppend(r, x::l)

    fun rev l = revAppend(l, [])

    exception ex_undefined of string
    fun error str = raise ex_undefined str

    fun accumulate f = let
	  fun foldf a [] = a
            | foldf a (b::x) = foldf (f a b) x
          in
	    foldf
	  end

    fun filter p = let
	  fun consifp x a = if p a then a::x else x
          in
	    rev o accumulate consifp []
	  end

    fun exists p = let fun existsp [] = false
                     | existsp (a::x) = if p a then true else existsp x
                in existsp end

    fun equal a b = (a  = b)

    fun member x a = exists (equal a) x

    fun C f x y = f y x

    fun cons a x = a::x

    fun revonto x = accumulate (C cons) x

    fun length x = let fun count n a = n+1 in accumulate count 0 x end

    fun repeat f = let fun rptf n x = if n=0 then x else rptf(n-1)(f x)
                       fun check n = if n<0 then error "repeat<0" else n
                    in rptf o check end

    fun copy n x = repeat (cons x) n []

    fun spaces n = concat (copy n " ")

    local
      fun lexordset [] = []
        | lexordset (a::x) = lexordset (filter (lexless a) x) @ [a] @
                             lexordset (filter (lexgreater a) x)
      and lexless(a1:int,b1:int)(a2,b2) =
           if a2<a1 then true else if a2=a1 then b2<b1 else false
      and lexgreater pr1 pr2 = lexless pr2 pr1
      fun collect f list =
             let fun accumf sofar [] = sofar
                   | accumf sofar (a::x) = accumf (revonto sofar (f a)) x
              in accumf [] list
             end
      fun occurs3 x =
          (* finds coords which occur exactly 3 times in coordlist x *)
          let fun f xover x3 x2 x1 [] = diff x3 xover
                | f xover x3 x2 x1 (a::x) =
                   if member xover a then f xover x3 x2 x1 x else
                   if member x3 a then f (a::xover) x3 x2 x1 x else
                   if member x2 a then f xover (a::x3) x2 x1 x else
                   if member x1 a then f xover x3 (a::x2) x1 x else
		                       f xover x3 x2 (a::x1) x
              and diff x y = filter (not o member y) x
           in f [] [] [] [] x end
     in
      abstype generation = GEN of (int*int) list
        with
          fun alive (GEN livecoords) = livecoords
          and mkgen coordlist = GEN (lexordset coordlist)
          and mk_nextgen_fn neighbours gen =
              let val living = alive gen
                  val isalive = member living
                  val liveneighbours = length o filter isalive o neighbours
                  fun twoorthree n = n=2 orelse n=3
	          val survivors = filter (twoorthree o liveneighbours) living
	          val newnbrlist = collect (filter (not o isalive) o neighbours) living
	          val newborn = occurs3 newnbrlist
	       in mkgen (survivors @ newborn) end
     end
    end

    fun neighbours (i,j) = [(i-1,j-1),(i-1,j),(i-1,j+1),
			    (i,j-1),(i,j+1),
			    (i+1,j-1),(i+1,j),(i+1,j+1)]

    local val xstart = 0 and ystart = 0
          fun markafter n string = string ^ spaces n ^ "0"
          fun plotfrom (x,y) (* current position *)
                       str   (* current line being prepared -- a string *)
                       ((x1,y1)::more)  (* coordinates to be plotted *)
              = if x=x1
                 then (* same line so extend str and continue from y1+1 *)
                      plotfrom(x,y1+1)(markafter(y1-y)str)more
                 else (* flush current line and start a new line *)
                      str :: plotfrom(x+1,ystart)""((x1,y1)::more)
            | plotfrom (x,y) str [] = [str]
           fun good (x,y) = x>=xstart andalso y>=ystart
     in  fun plot coordlist = plotfrom(xstart,ystart) ""
                                 (filter good coordlist)
    end


    infix 6 at
    fun coordlist at (x:int,y:int) = let fun move(a,b) = (a+x,b+y)
                                      in map move coordlist end
    val rotate = map (fn (x:int,y:int) => (y,~x))

    val glider = [(0,0),(0,2),(1,1),(1,2),(2,1)]
    val bail = [(0,0),(0,1),(1,0),(1,1)]
    fun barberpole n =
       let fun f i = if i=n then (n+n-1,n+n)::(n+n,n+n)::nil
                       else (i+i,i+i+1)::(i+i+2,i+i+1)::f(i+1)
        in (0,0)::(1,0):: f 0
       end

    val genB = mkgen(glider at (2,2) @ bail at (2,12)
		     @ rotate (barberpole 4) at (5,20))

    fun nthgen g 0 = g | nthgen g i = nthgen (mk_nextgen_fn neighbours g) (i-1)

    val gun = mkgen
     [(2,20),(3,19),(3,21),(4,18),(4,22),(4,23),(4,32),(5,7),(5,8),(5,18),
      (5,22),(5,23),(5,29),(5,30),(5,31),(5,32),(5,36),(6,7),(6,8),(6,18),
      (6,22),(6,23),(6,28),(6,29),(6,30),(6,31),(6,36),(7,19),(7,21),(7,28),
      (7,31),(7,40),(7,41),(8,20),(8,28),(8,29),(8,30),(8,31),(8,40),(8,41),
      (9,29),(9,30),(9,31),(9,32)]

    fun show pr = (app (fn s => (pr s; pr "\n"))) o plot o alive

    fun runOnce () = nthgen gun 50

    fun doit () = let
          fun lp 0 = ()
            | lp n = (runOnce(); lp(n-1))
          in
            lp 1000
          end

    fun testit () = show Log.print (runOnce())

  end (* Main *)
end

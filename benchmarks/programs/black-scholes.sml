(* all.sml -- all sources for black-scholes *)
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
(******************** black-scholes.sml ********************)
(* black-scholes.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure BlackScholes : sig

    datatype option_ty = Put | Call

    datatype option_t = Option of {
	spot : real,          (* spot price *)
	strike : real,        (* strike price *)
	interest : real,      (* risk-free interest rate *)
	div_rate : real,      (* dividend rate *)
	volatility : real,    (* volatility *)
	time : real,          (* years to maturity or expiration: 6mos = .5, etc. *)
	opt_type : option_ty,(* Put or Call *)
	div_vals : real,      (* dividend values (not used here) *)
	derivagem : real      (* expected answer from DerivaGem *)
      }

    val dummy_option : option_t

    val readData : string -> option_t list

    val price : option_t -> real

  end = struct

    datatype option_ty = Put | Call

    datatype option_t = Option of {
	spot : real,          (* spot price *)
	strike : real,        (* strike price *)
	interest : real,      (* risk-free interest rate *)
	div_rate : real,      (* dividend rate *)
	volatility : real,    (* volatility *)
	time : real,          (* years to maturity or expiration: 6mos = .5, etc. *)
	opt_type : option_ty,(* Put or Call *)
	div_vals : real,      (* dividend values (not used here) *)
	derivagem : real      (* expected answer from DerivaGem *)
      }

    val dummy_option = Option{
	    spot=123.0, strike=234.0, interest=345.0,
	    div_rate=456.0, volatility=567.0, time=678.0,
	    opt_type=Put, div_vals=789.0,
	    derivagem=890.0
	  }

  (* readData : string -> option_t list *)
  fun readData (infname : string) : option_t list = let
        fun fromString s = Option.valOf (Real.fromString s)
        val inS = TextIO.openIn infname
        val numOptions =
              (Option.valOf o Option.composePartial(Int.fromString, TextIO.inputLine)) inS
        (* string list -> option_t *)
        fun fieldsToOption [
                spot, strike, interest, div_rate, volatility,
                time, opt_type, div_vals, derivagem
              ] = Option {
                  spot=fromString spot,
                  strike=fromString strike,
                  interest=fromString interest,
                  div_rate=fromString div_rate,
                  volatility=fromString volatility,
                  time=fromString time,
                  (* TODO throw exception if neither "P" nor "C" *)
                  opt_type= if opt_type = "P" then Put else Call,
                  div_vals=fromString div_vals,
                  derivagem=fromString derivagem
                }
          | fieldsToOption flds = raise Fail "fieldsToOption: bogus fields"
        val options = let
              fun lp opts = (case TextIO.inputLine inS
                     of NONE => List.rev opts
                      | SOME ln =>
                          lp (fieldsToOption (String.tokens Char.isSpace ln) :: opts)
                    (* end case *))
              in
                lp [] handle ex => (TextIO.closeIn inS; raise ex)
              end
        val _ = TextIO.closeIn inS
        in
          if numOptions = length options
            then options
            else raise Fail "number of options does not match input"
        end

    (* uses Horner's algorithm to evaluate a polynomial whose coefficients are
     * specified in order of ascending degree: x + 2 is [2, 1]
     *)
    fun poly (c::coeffs) (x : real) =
          List.foldl (fn (c, partial) => c + x * partial) c coeffs

    local
      (* probability density function for standard normal distribution:
       * mean = 0, variance = 1 *)
      val normalization_factor = 1.0 / Math.sqrt(2.0 * Math.pi)
      fun std_normal_pdf x = normalization_factor * Math.exp (~0.5 * x * x)

      (* Approximation of the normal cumulative density function for x > 0 which
       * gives error < 7.5e-8. Algorithm 26.3.7 in Abramowitz and Stegun (1964) *)
      (* magic constants specified in order b5, b4, b3, b2, b1, 0.0 *)
      val magic = [
              1.330274429, ~1.821255978, 1.781477937, ~0.356563782,
              0.319381530, 0.0
            ]
      val b0 = 0.2316419
    in
    fun magic_poly x = poly magic (1.0 / (1.0 + b0 * x))
    (* cumulative density function for standard normal distribution *)
    fun std_normal_cdf x = if x < 0.0
          then (std_normal_pdf (~x)) * (magic_poly (~x))
          else 1.0 - (std_normal_pdf x) * (magic_poly x)
    end (* local *)

    fun price opt = let
          val Option{
                  spot, strike, interest, volatility, time, opt_type, derivagem, ...
                } = opt
          val denom = volatility * Math.sqrt (time)
          val strike_exp = strike * Math.exp (~interest * time)
          val log_term = Math.ln (spot / strike)
          val time_term = (interest + (volatility * volatility * 0.5)) * time
          val d1 = (log_term + time_term) / denom
          val d2 = d1 - denom
          val n_d1 = std_normal_cdf d1
          val n_d2 = std_normal_cdf d2
          val retval = (case opt_type
                 of Put => strike_exp * (1.0 - n_d2) - spot * (1.0 - n_d1)
                  | Call => spot * n_d1 - strike_exp * n_d2
                (* end case *))
          in
            Real.abs (retval - derivagem)
	  end

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

    val name = "black-scholes"

    val results : string list = []

    (* preload the data for the timing tests; the "large" data set has 64K entries;
     * we scale that up to 2M entries by concatenating the list.
     *)
    val data = let
          val data = BlackScholes.readData "DATA/simlarge.txt"
          in
            List.concat(List.tabulate(32, fn _ => data))
          end

    fun testit () = let
          fun f opt = Log.say [Real.toString(BlackScholes.price opt), "\n"]
          in
            List.app f (BlackScholes.readData "DATA/simsmall.txt")
          end

    fun doOne () = let
          val prices = List.map BlackScholes.price data
	  in
	    Log.say [Real.toString(hd prices), "\n"]
	  end

    fun loop n = if n <= 0 then () else (doOne (); loop (n - 1))

    fun doit () = loop 10

  end
end

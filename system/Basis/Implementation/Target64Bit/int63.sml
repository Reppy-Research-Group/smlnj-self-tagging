(* int.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Default int structure (63 bits) for 64-bit targets.
 *)

structure Int63Imp : INTEGER =
  struct
    structure I63 = InlineT.Int63

    exception Div = Assembly.Div
    exception Overflow = Assembly.Overflow

    type int = Int63.int

    val precision = SOME 63
    val minIntVal : int = ~4611686018427387904
    val minInt : int option = SOME minIntVal
    val maxInt : int option = SOME 4611686018427387903

    val toLarge : int -> LargeInt.int = I63.toLarge
    val fromLarge : LargeInt.int -> int = I63.fromLarge
    val toInt = I63.toInt
    val fromInt = I63.fromInt

    val ~ 	: int -> int = I63.~
    val op * 	: int * int -> int  = I63.*
    val op + 	: int * int -> int  = I63.+
    val op - 	: int * int -> int  = I63.-
    val op div 	: int * int -> int  = I63.div
    val op mod 	: int * int -> int  = I63.mod
    val op quot : int * int -> int  = I63.quot
    val op rem 	: int * int -> int  = I63.rem
    val min 	: int * int -> int  = I63.min
    val max 	: int * int -> int  = I63.max
    val abs 	: int -> int = I63.abs

    fun sign 0 = 0
      | sign i = if I63.<(i, 0) then ~1 else 1

    fun sameSign (i,j) = (sign i = sign j)

    fun compare (i, j) =
	  if (I63.<(i, j)) then General.LESS
	  else if (I63.>(i, j)) then General.GREATER
	  else General.EQUAL
    val op > 	: int * int -> bool = I63.>
    val op >= 	: int * int -> bool = I63.>=
    val op < 	: int * int -> bool = I63.<
    val op <= 	: int * int -> bool = I63.<=

    fun fmt radix = (NumFormat64.fmtInt radix) o toInt

    fun scan radix = let
	  val scanInt64 = NumScan64.scanInt radix
	  fun f getc cs = (case scanInt64 getc cs
		   of NONE => NONE
		    | SOME(i, cs') => SOME(fromInt i, cs')
		  (* end case *))
	  in
	    f
	  end

    val toString = fmt StringCvt.DEC

(*
    val fromString = PreBasis.scanString (scan StringCvt.DEC)
*)
    local
      structure Word = InlineT.Word
      structure CV = InlineT.CharVector
    in
  (* optimized version of fromString; it is about 2x as fast as
   * using scanString:
   *)
    fun fromString s = let
	  val n = size s
	  val z = ord #"0"
	  val sub = CV.sub

          (* Indices are always the *default* integer *)
	  infix ++
	  fun x ++ y = InlineT.Int.fast_add(x, y)
          fun x * y = InlineT.Int.* (x, y)
          fun x - y = InlineT.Int.- (x, y)
          val op >= = InlineT.Int.>=
          val op > = InlineT.Int.<
          val op < = InlineT.Int.<
          val op ~ = InlineT.Int.~

	  fun num (i, a) = if i >= n
		then a
		else let
		  val c = ord (sub (s, i)) - z
		  in
		    if c < 0 orelse c > 9
		      then a
		      else num (i ++ 1, 10 * a - c)
		  end
	(* Do the arithmetic using the negated absolute to avoid
	 * premature overflow on minInt.
	 *)
	  fun negabs i = if i >= n
		then NONE
		else let
		  val c = z - ord (sub (s, i))
		  in
		    if c > 0 orelse c < ~9
		      then NONE
		      else SOME (num (i ++ 1, c))
		  end
	  fun skipwhite i = if i >= n
		then NONE
	        else let
		  val c = sub (s, i)
		  in
		    if Char.isSpace c
		      then skipwhite (i ++ 1)
		    else if c = #"-" orelse c = #"~"
		      then negabs (i ++ 1)
		    else if c = #"+"
		      then Option.map ~ (negabs (i ++ 1))
		      else Option.map ~ (negabs i)
		  end
	  in
	    Option.map fromInt (skipwhite 0)
	  end
    end (* local *)

  end  (* structure IntImp *)

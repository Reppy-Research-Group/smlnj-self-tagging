(* testcnv.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 *	#define MAX_TO_INT	((1 << (to - 1)) - 1)
 *	#define MIN_TO_INT	-maxToInt - 1
 *
 *	to_int test (from_int x)
 *	{
 *	    if ((x < MIN_TO_INT) || (MAX_TO_INT < x))
 *		raise Overflow;
 *	    else
 *		return (to_int)x;
 *	}
 *
 *	to_int testu (from_uint x)
 *	{
 *	    if ((unsigned)x <= (unsigned)MAX_TO_INT)
 *		return (to_int)x;
 *	    else
 *		raise Overflow;
 *	}
 *)

structure TestCnv : sig

  (* lower a `TEST` (int -> int conversion) to explicit bounds checks; we expect
   * that the `to` type will be a tagged size.
   *)
    val test : int * int * CPS.value list * CPS.lvar * CPS.cty * CPS.cexp -> CPS.cexp

  (* lower a `TESTU` (word -> int conversion) to explicit bounds checks. *)
    val testu : int * int * CPS.value list * CPS.lvar * CPS.cty * CPS.cexp -> CPS.cexp

  end = struct

    structure C = CPS
    structure P = C.P
    structure LV = LambdaVar

    fun bug s = ErrorMsg.impossible ("TestCnv: " ^ s)

  (* bit width of target word (32 or 64) *)
    val ity = Target.mlValueSz
  (* bit width of default tagged integer size (31 or 63) *)
    val tty = Target.defaultTaggedIntSz

    fun sLT sz = P.CMP{oper=P.LT, kind=P.INT sz}
    fun uLE sz = P.CMP{oper=P.LTE, kind=P.UINT sz}
    fun branch (cmp, args, k1, k2) = C.BRANCH(cmp, args, LV.mkLvar(), k1, k2)

    val mlUnit = C.ENUM 0

    fun mkTrap k = let
	  val tmp = LV.mkLvar()
	  val ty = {tag=false, sz=ity}
	  val n = C.NUM{ival = IntInf.<<(1, Word.fromInt(ity-1)), ty = ty}
	  in
	    C.ARITH(P.IARITH{oper=P.IADD, sz=ity}, [n, n], tmp, C.NUMt ty, k)
	  end

    fun test (from, to, [v], x, ty, k) =
	  if (from = ity) andalso (to = tty)
	    then C.ARITH(P.TEST{from=from, to=to}, [v], x, ty, k)
	  else if (from < to)
	    then C.PURE(P.EXTEND{from=from, to=to}, [v], x, ty, k)
	  else if (from = to)
	    then C.PURE(P.COPY{from=from, to=to}, [v], x, ty, k)
	  else if (from <= ity) andalso (to < tty)
	    then let
	      val fromIsTagged = Target.isTaggedIntSz from
	      fun num iv = C.NUM{ival=iv, ty={sz=from, tag=fromIsTagged}}
	      val maxToInt = IntInf.<<(1, Word.fromInt(to - 1)) - 1
	      val minToInt = ~(maxToInt + 1)
	      val jk = LV.mkLvar()
	      val jk' = C.VAR jk
	      fun narrow v = let
		    val x' = LV.mkLvar()
		    in
		      C.PURE(P.TRUNC{from=from, to=to}, [v], x', ty,
			C.APP(jk', [C.VAR x']))
		    end
(*
	      val trap = C.TRAP(C.APP(jk', [v]))
*)
	      val trap = mkTrap (narrow v)
	      in
		C.FIX([(C.CONT, jk, [x], [ty], k)],
		  branch(sLT from, [v, num minToInt],
		    trap,
		    branch(sLT from, [num maxToInt, v],
		      trap,
		      narrow v)))
	      end
	    else bug "TEST with unexpected precisions"
      | test _ = bug "TEST with bogus arguments"

    fun testu (from, to, [v], x, ty, k) =
	  if (from = to) andalso ((from = ity) orelse (from = tty))
	    then C.ARITH(P.TESTU{from=from, to=to}, [v], x, ty, k)
	  else if (from < to)
	    then C.PURE(P.COPY{from=from, to=to}, [v], x, ty, k)
	  else let
	      val fromIsTagged = Target.isTaggedIntSz from
	      fun num iv = C.NUM{ival=iv, ty={sz=from, tag=fromIsTagged}}
	      val maxToInt = IntInf.<<(1, Word.fromInt(to - 1)) - 1
	      val jk = LV.mkLvar()
	      val jk' = C.VAR jk
	      fun narrow v = let
		    val x' = LV.mkLvar()
		    val oper = if (from = to)
			  then P.COPY{from=from, to=to}
			  else P.TRUNC{from=from, to=to}
		    in
		      C.PURE(oper, [v], x', ty, C.APP(jk', [C.VAR x']))
		    end
	      in
		C.FIX([(C.CONT, jk, [x], [ty], k)],
		  branch(uLE from, [v, num maxToInt],
		    narrow v,
(*
		    C.TRAP(C.APP(jk', [v]))))
*)
		    mkTrap (narrow v)))
	      end
      | testu _ = bug "TESTU with bogus arguments"

  end

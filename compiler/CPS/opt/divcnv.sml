(* divcnv.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The SML Int.div and Int.mod operations round to negative infinity, with
 * the properties:
 *
 *	x div y		= floor(x / y)
 *	x		= y * (x div y) + (x mod y)
 *
 * The signs of the of the arguments and results satisfy the following table:
 *
 *      x       y      (x div y)     (x mod y)
 *     ---     ---     ---------     ---------
 *      +       +          +             +
 *      +       -          -             -
 *      -       +          -             +
 *      -       -          +             -
 *
 * When the signs of x and y are the same, then Int.div and Int.mod return the
 * same results as Int.quot and Int.rem (resp.), but when the signs are different,
 * we must xorArgs the result (assuming that the remainder is non-zero).
 *
 * We use the following implementation:
 *
 *    int div ( int D, int d )
 *    {
 *      int q = D / d;
 *      int r = D % d;
 *	if (((x ^ y) < 0) && (r != 0)) q = q-1;
 *      return q;
 *    }
 *
 *    int mod ( int D, int d )
 *    {
 *      int r = D % d;
 *      if (((x ^ y) < 0) && (r != 0)) r = r+d;
 *      return r;
 *    }
 *
 * For the LLVM backend, the `/` and `%` operations in `div` will be be combined into
 * a single `/` operation (that produces both results).
 *)

structure DivCnv : sig

  (* expand the `div` operation to use native machine arithmetic. *)
    val expandDiv : CPS.value list * CPS.lvar * CPS.cty * CPS.cexp -> CPS.cexp

  (* expand the `mod` operation to use native machine arithmetic. *)
    val expandMod : CPS.value list * CPS.lvar * CPS.cty * CPS.cexp -> CPS.cexp

  end = struct

    structure C = CPS
    structure P = C.P
    structure LV = LambdaVar

    fun bug s = ErrorMsg.impossible ("DivCnv: " ^ s)

    datatype sign = N | P | U	(* negative, positive, unknown *)

  (* bit-width of target word (32 or 64) *)
    val ity = Target.mlValueSz
  (* bit-width of default tagged integer type *)
    val tty = Target.defaultTaggedIntSz

    val intTy = C.NUMt{sz = ity, tag=false}

    val tagTy = C.NUMt{sz = tty, tag=true}

    fun numTy sz = C.NUMt{sz=sz, tag=false}

    fun zero sz =
        C.NUM{ival=0, ty={sz=sz, tag=false}}

    fun one sz =
        C.NUM{ival=1, ty={sz=sz, tag=false}}

    fun num sz iv =
        C.NUM{ival=iv, ty={sz=sz, tag=false}}

    fun arith (sz, oper, args, res, ty, k) =
        C.ARITH(P.IARITH{oper=oper, sz=sz}, args, res, ty, k)

    fun branch (sz, cmp, args, k1, k2) =
        C.BRANCH(
          P.CMP{oper=cmp, kind=P.INT sz},
          args, LV.mkLvar(), k1, k2)

    fun signOf (C.NUM{ival, ...}) = if (ival < 0) then N else P
      | signOf _ = U

    fun pure (oper, kind, args, res, ty, k) =
	  C.PURE(P.PURE_ARITH{oper=oper, kind=kind}, args, res, ty, k)

    fun untag (x, x', k) =
	  C.PURE(P.EXTEND{from=tty, to=ity}, [x], x', intTy, k)

    fun tag (x, x', k) =
	  C.ARITH(P.TEST{from=ity, to=tty}, [x], x', tagTy, k)

    fun expandDiv' (sz, purePrim, x, y, res, k) = let
          val ty = numTy sz
          val zero = zero sz
          val one = one sz

	  fun divide k = let
		val q = LV.mkLvar()
		val r = LV.mkLvar()
		in
		  if purePrim
		    then pure(P.QUOT, P.INT sz, [x, y], q, ty,
		      pure(P.REM, P.INT sz, [x, y], r, ty,
			k (C.VAR q, C.VAR r)))
		    else arith(sz, P.IQUOT, [x, y], q, ty,
		      arith(sz, P.IREM, [x, y], r, ty,
			k (C.VAR q, C.VAR r)))
		end
	  fun xorArgs k (q, r) = let
		val sgn = LV.mkLvar()
		in
		  pure(P.XORB, P.UINT sz, [x, y], sgn, ty, k (C.VAR sgn) (q, r))
		end
	  fun chkSign (tst, args) (q, r) = let
		val jk = LV.mkLvar()
		val q' = LV.mkLvar()
		in
		  C.FIX([(C.CONT, jk, [res], [ty], k)],
		    branch (sz, tst, args,
		      branch (sz, P.NEQ, [r, zero],
			pure (P.SUB, P.INT sz, [q, one], q', ty,
			  C.APP(C.VAR jk, [C.VAR q'])),
			C.APP(C.VAR jk, [q])),
		      C.APP(C.VAR jk, [q])))
		end
	  in
	  (* when one of the arguments is a known constant, we can simplify the
	   * sign test to avoid the XORB operation.
	   *)
	    case (signOf x, signOf y)
	     of (U, N) => divide (chkSign (P.GTE, [x, zero]))
	      | (U, P) => divide (chkSign (P.LT, [x, zero]))
	      | (N, U) => divide (chkSign (P.GTE, [y, zero]))
	      | (P, U) => divide (chkSign (P.LT, [y, zero]))
	    (* Note: we subsume the case where both arguments are known into the
	     * general case, since that will be a very rare occurrence (i.e., if
	     * there will be an overflow or if contraction is disabled), so it
	     * is not worth special handling.
	     *)
	      | _ => divide (xorArgs (fn sgn => chkSign (P.LT, [sgn, zero])))
	    (* end case *)
	  end (* expandDiv *)

    fun expandDiv ([_, C.NUM{ival=0, ...}], _, _, _) = bug "impossible divide by zero"
      | expandDiv ([x, y], res, C.NUMt{sz, tag=true}, k) = let
	(* division of tagged numbers, so we wrap it with untagging/tagging code *)
	  val res' = LV.mkLvar()
	  val tagResExp = tag (C.VAR res', res, k)
	  fun untagY x' = (case y
		 of C.NUM{ival, ...} => expandDiv' (ity, true, x', num ity ival, res', tagResExp)
		  | _ => let
		      val y' = LV.mkLvar()
		      in
			untag (y, y',
			  expandDiv' (ity, true, x', C.VAR y', res', tagResExp))
		      end
		(* end case *))
	  in
	    case x
	     of C.NUM{ival, ...} => untagY (num ity ival)
	      | _ => let
		  val x' = LV.mkLvar()
		  in
		    untag (x, x', untagY (C.VAR x'))
		  end
	    (* end case *)
	  end
      | expandDiv ([x, y], res, C.NUMt{sz, ...}, k) =
	    expandDiv' (sz, false, x, y, res, k)
      | expandDiv _ = bug "expandDiv: bogus arguments"

    fun expandMod' (sz, purePrim, x, y, res, k) = let
          val ty = numTy sz
          val zero = zero sz
	  fun modulo k = let
		val r = LV.mkLvar()
		in
		  if purePrim
		    then pure(P.REM, P.INT sz, [x, y], r, ty, k(C.VAR r))
		    else arith(sz, P.IREM, [x, y], r, ty, k(C.VAR r))
		end
	  fun xorArgs k r = let
		val sgn = LV.mkLvar()
		in
		  pure(P.XORB, P.UINT sz, [x, y], sgn, ty, k (C.VAR sgn) r)
		end
	  fun chkSign (tst, args) r = let
		val jk = LV.mkLvar()
		val r' = LV.mkLvar()
		in
		  C.FIX([(C.CONT, jk, [res], [ty], k)],
		    branch (sz, tst, args,
		      branch (sz, P.NEQ, [r, zero],
			pure (P.ADD, P.INT sz, [r, y], r', ty,
			  C.APP(C.VAR jk, [C.VAR r'])),
			C.APP(C.VAR jk, [r])),
		      C.APP(C.VAR jk, [r])))
		end
	  in
	  (* when one of the arguments is a known constant, we can simplify the
	   * sign test to avoid the XORB operation.
	   *)
	    case (signOf x, signOf y)
	     of (U, N) => modulo (chkSign (P.GTE, [x, zero]))
	      | (U, P) => modulo (chkSign (P.LT, [x, zero]))
	      | (N, U) => modulo (chkSign (P.GTE, [y, zero]))
	      | (P, U) => modulo (chkSign (P.LT, [y, zero]))
	    (* Note: we subsume the case where both arguments are known into the
	     * general case, since that will be a very rare occurrence (i.e., if
	     * contraction is disabled), so it is not worth special handling.
	     *)
	      | _ => modulo (xorArgs (fn sgn => chkSign (P.LT, [sgn, zero])))
	    (* end case *)
	  end

    fun expandMod ([_, C.NUM{ival=0, ...}], _, _, _) = bug "impossible modulo zero"
      | expandMod ([x, y], res, C.NUMt{sz, tag=true}, k) = let
	(* modulo of tagged numbers, so we wrap it with untagging/tagging code *)
	  val res' = LV.mkLvar()
	  val tagResExp = tag (C.VAR res', res, k)
	  fun untagY x' = (case y
		 of C.NUM{ival, ...} => expandMod' (ity, true, x', num ity ival, res', tagResExp)
		  | _ => let
		      val y' = LV.mkLvar()
		      in
			untag (y, y',
			  expandMod' (ity, true, x', C.VAR y', res', tagResExp))
		      end
		(* end case *))
	  in
	    case x
	     of C.NUM{ival, ...} => untagY (num ity ival)
	      | _ => let
		  val x' = LV.mkLvar()
		  in
		    untag (x, x', untagY (C.VAR x'))
		  end
	    (* end case *)
	  end
      | expandMod ([x, y], res, C.NUMt{sz, ...}, k) =
	  expandMod' (sz, false, x, y, res, k)
      | expandMod _ = bug "expandMod: bogus arguments"

  end

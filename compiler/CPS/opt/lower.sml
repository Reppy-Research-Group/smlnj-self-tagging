(* lower.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * This module implements a lowering pass for the CPS IR that should
 * be run after optimization, but before closure conversion.  The lowering
 * pass makes the following changes:
 *
 *	- expand 64-bit arithmetic to operations on pairs of 32-bit words
 *	  (on 32-bit targets only).
 *
 *	- replace conversions that involve the `IntInf.int` type with
 *	  calls to the pre-basis library code (see `system/smlnj/init`),
 *
 *	- replace the `round-to-negative-infinity` division operations
 *	  (i.e., `div` and `mod`) with native arithmetic operations.
 *
 *	- replace `TEST` and `TESTU` narrowing conversions (other than
 *	  from the native integer size to the tagged integer size) with
 *	  code that tests for overflow and explicitly raises the exception
 *	  if necessary.
 *
 *	- replace `STREQL` with unrolled sequence of word-sized equality
 *	  tests.
 *
 *      - replace `ROTL` and `ROTR` operations on tagged words with shifts
 *
 *      - expand `UNWRAP` and `WRAP` operations on integers
 *
 * Note that the bulk of the work is actually done in other modules; this
 * module is responsible for detecting places where things need transforming.
 *)

structure LowerCPS : sig

    val transform : CPS.function -> CPS.function

  end = struct

    structure C = CPS
    structure P = C.P
    structure LV = LambdaVar

    fun bug s = ErrorMsg.impossible ("LowerCPS: " ^ s)

    val ity = Target.mlValueSz

    fun numTy sz = C.NUMt{sz = sz, tag = Target.isTaggedIntSz sz}
    val i64Ty = C.NUMt{sz = 64, tag = false}

    (* support for pure arithmetic operations on words (used in the
     * expansion of ROTL and ROTR on tagged types).
     *)
    fun pureOp (oper, sz) = P.PURE_ARITH{oper=oper, kind=P.UINT sz}
    fun letPure (oper, sz, args : C.value list, k : C.value -> C.cexp) = let
          val x = LV.mkLvar()
          in
            C.PURE(pureOp(oper, sz), args, x, numTy sz,
              k (C.VAR x))
          end

    fun letCAST (arg, toTy, k) = let
          val x = LV.mkLvar()
          in
            C.PURE(P.CAST, [arg], x, toTy, k (C.VAR x))
          end

    fun num (i, sz) =
          C.NUM{ival = IntInf.fromInt i, ty = {sz = sz, tag = Target.isTaggedIntSz sz}}
    fun num' (w, sz) =
          C.NUM{ival = Word.toLargeInt w, ty = {sz = sz, tag = Target.isTaggedIntSz sz}}
    fun num64 i = C.NUM{ival = i, ty = {sz = 64, tag = false}}

    fun transform cfun = let
	  fun function (fk, f, formals, tl, e) = let
		fun cexp (C.RECORD(rk, xl, v, e)) =
		      C.RECORD(rk, xl, v, cexp e)
		  | cexp (C.SELECT(i, x, v, t, e)) =
		      C.SELECT(i, x, v, t, cexp e)
		  | cexp (C.OFFSET(i, v, x, e)) =
		      C.OFFSET(i, v, x, cexp e)
		  | cexp (C.APP(x, xl)) =
		      C.APP(x, xl)
		  | cexp (C.FIX(fl, e)) =
		      C.FIX(map function fl, cexp e)
		  | cexp (C.SWITCH(x, v, el)) =
		      C.SWITCH(x, v, map cexp el)
		  | cexp (C.BRANCH(P.STREQL lit, [s], _, e1, e2)) =
		      StrEqlCnv.strEql (s, lit, cexp e1, cexp e2)
		  | cexp (C.BRANCH(P.STREQL _, _, _, _, _)) = bug "bogus STREQL"
		  | cexp (C.BRANCH(b, xl, v, e1, e2)) =
		      C.BRANCH(b, xl, v, cexp e1, cexp e2)
		  | cexp (C.SETTER(s, xl, e)) =
		      C.SETTER(s, xl, cexp e)
		  | cexp (C.LOOKER(l, xl, v, t, e)) =
		      C.LOOKER(l, xl, v, t, cexp e)
		  | cexp (C.PURE(P.COPY_INF sz, args, v, t, e)) =
		      IntInfCnv.toInf (P.COPY, sz, args, v, t, cexp e)
		  | cexp (C.PURE(P.EXTEND_INF sz, args, v, t, e)) =
		      IntInfCnv.toInf (P.EXTEND, sz, args, v, t, cexp e)
		  | cexp (C.PURE(P.TRUNC_INF sz, args, v, t, e)) =
		      IntInfCnv.truncInf (sz, args, v, t, cexp e)
		  | cexp (C.PURE(p as P.PURE_ARITH{oper=P.ROTL, kind=P.UINT sz}, [v1, v2], x, t, e)) =
                      if Target.isTaggedIntSz sz
                        then let
                          (* for rotations of tagged types, we expand to
                           *    `(v1 >> k) | ((v1 & m) << n)`
                           * where
                           *    n = v2 mod sz
                           *    k = (sz - n)
                           *    m = ((1 << k) - 1)
                           *)
                          fun rot (n, k, m) =
                                letPure (P.RSHIFT, sz, [v1, k], fn lo =>
                                letPure (P.ANDB, sz, [v1, m], fn tmp =>
                                letPure (P.LSHIFT, sz, [tmp, n], fn hi =>
                                  C.PURE(pureOp(P.ORB, sz), [lo, hi], x, t, cexp e))))
                          in
                            (* specialize for when the rotation amount is a constant *)
                            case v2
                             of C.NUM{ival, ...} => let
                                (* contraction guarantees 0 < v2 < sz *)
                                val k = Word.fromInt(sz - IntInf.toInt ival)
                                val m = Word.<<(0w1, k) - 0w1
                                in
                                  rot (v2, num'(k, sz), num'(m, sz))
                                end
                              | _ =>
                                letPure (P.REM, sz, [v2, num(sz, sz)], fn n =>
                                letPure (P.SUB, sz, [num (sz, sz), n], fn k =>
                                letPure (P.LSHIFT, sz, [num(1, sz), k], fn tmp =>
                                letPure (P.SUB, sz, [tmp, num(1, sz)], fn m =>
                                  rot (n, k, m)))))
                            (* end case *)
                          end
                        (* rotations of native words are handled by hardware *)
                        else C.PURE(p, [v1, v2], x, t, cexp e)
		  | cexp (C.PURE(p as P.PURE_ARITH{oper=P.ROTR, kind=P.UINT sz}, [v1, v2], x, t, e)) =
                      if Target.isTaggedIntSz sz
                        then let
                          (* for rotations of tagged types, we expand to
                           *    `(v1 >> n) | ((v1 & m) << k)`
                           * where
                           *    n = v2 mod sz
                           *    k = (sz - n)
                           *    m = ((1 << n) - 1)
                           *)
                          fun rot (n, k, m) =
                                letPure (P.RSHIFT, sz, [v1, n], fn lo =>
                                letPure (P.ANDB, sz, [v1, m], fn tmp =>
                                letPure (P.LSHIFT, sz, [tmp, k], fn hi =>
                                  C.PURE(pureOp(P.ORB, sz), [lo, hi], x, t, cexp e))))
                          in
                            (* specialize for when the rotation amount is a constant *)
                            case v2
                             of C.NUM{ival, ...} => let
                                (* contraction guarantees 0 < v2 < sz *)
                                val n' = IntInf.toInt ival
                                val k = Word.fromInt(sz - n')
                                val m = Word.<<(0w1, Word.fromInt n') - 0w1
                                in
                                  rot (v2, num'(k, sz), num'(m, sz))
                                end
                              | _ =>
                                letPure(P.REM, sz, [v2, num (sz, sz)], fn n =>
                                letPure (P.SUB, sz, [num (sz, sz), n], fn k =>
                                letPure (P.LSHIFT, sz, [num(1, sz), n], fn t =>
                                letPure (P.SUB, sz, [t, num(1, sz)], fn m =>
                                  rot (n, k, m)))))
                            (* end case *)
                          end
                        (* rotations of native words are handled by hardware *)
                        else C.PURE(p, [v1, v2], x, t, cexp e)
                  | cexp (C.PURE(P.WRAP(P.INT 64), [v], x, t, e)) = let
(* TODO: add a control to enable/disable self tagging *)
                      val joinK = LV.namedLvar' "join"
                      val res' = LV.mkLvar()
                      in
                        C.FIX([(C.KNOWN_CONT, joinK, [x], [t], cexp e)],
                          letPure (P.ROTL, 64, [v, num64 3], fn y =>
                          letPure (P.XORB, 64, [y, num64 6], fn z =>
                            C.BRANCH(P.RAW, [z], LV.mkLvar(),
                              (* true *)
                              letCAST (z, t, fn res => C.APP(C.VAR joinK, [res])),
                              (* false *)
                              C.PURE(P.WRAP(P.INT 64), [v], res', t,
                                C.APP(C.VAR joinK, [C.VAR res']))))))
                      end
                  | cexp (C.PURE(P.WRAP(P.INT sz), [v], x, t, e)) = let
                      (* for smaller integer sizes, we use a tagged representation *)
                      val y = LV.mkLvar()
                      in
                        C.PURE(P.COPY{from=sz, to=64}, [v], y, i64Ty,
                        letPure (P.LSHIFT, 64, [C.VAR y, num64 1], fn z =>
                        letPure (P.ORB, 64, [z, num64 1], fn res =>
                        C.PURE(P.CAST, [res], x, t,
                          cexp e))))
                      end
                  | cexp (C.PURE(P.UNWRAP(P.INT 64), [v], x, t, e)) = let
                      val joinK = LV.namedLvar' "join"
                      val y = LV.mkLvar()
                      in
                        C.FIX([(C.KNOWN_CONT, joinK, [x], [t], cexp e)],
                          C.BRANCH(P.BOXED, [v], LV.mkLvar(),
                            (* true *)
                            C.PURE(P.UNWRAP(P.INT 64), [v], y, t,
                              C.APP(C.VAR joinK, [C.VAR y])),
                            (* false *)
                            letCAST(v, i64Ty, fn z =>
                            letPure(P.XORB, 64, [z, num64 6], fn w =>
                            letPure(P.ROTR, 64, [w, num64 3], fn u =>
                              C.APP(C.VAR joinK, [u]))))))
                      end
                  | cexp (C.PURE(P.UNWRAP(P.INT sz), [v], x, t, e)) =
                      letCAST (v, i64Ty, fn y =>
                      letPure (P.RSHIFTL, 64, [y, num64 1], fn z =>
                      C.PURE(P.TRUNC{from=64, to=sz}, [z], x, t,
                        cexp e)))
		  | cexp (C.ARITH(P.TEST_INF sz, args, v, t, e)) =
		      IntInfCnv.testInf (sz, args, v, t, cexp e)
		  | cexp (C.ARITH(P.TEST{from, to}, args, v, t, e)) =
		      TestCnv.test(from, to, args, v, t, cexp e)
		  | cexp (C.ARITH(oper as P.TESTU{from, to}, args, v, t, e)) =
		      TestCnv.testu(from, to, args, v, t, cexp e)
		  | cexp (C.ARITH(P.IARITH{oper=P.IDIV, sz}, xl, v, t, e)) =
		      DivCnv.expandDiv (xl, v, t, cexp e)
		  | cexp (C.ARITH(P.IARITH{oper=P.IMOD, sz}, xl, v, t, e)) =
		      DivCnv.expandMod (xl, v, t, cexp e)
		  | cexp (C.ARITH(a, xl, v, t, e)) = C.ARITH(a, xl, v, t, cexp e)
		  | cexp (C.PURE(p, xl, v, t, e)) = C.PURE(p, xl, v, t, cexp e)
		  | cexp (C.RCC(k, s, p, xl, vtl, e)) = C.RCC(k, s, p, xl, vtl, cexp e)
		in
		  (fk, f, formals, tl, cexp e)
		end
	  in
	  (* we expand 64-bit operations first (if required) and then do the rest *)
	    function (Num64Cnv.elim cfun)
	  end

  end

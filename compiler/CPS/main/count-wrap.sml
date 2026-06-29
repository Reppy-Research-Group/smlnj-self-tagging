structure InstrumentWrap :> sig
  val instrument : CPS.function -> CPS.function
end = struct
  structure LV = LambdaVar
  open CPS
  val tmpName = Symbol.varSymbol "instrument"
  fun var () = LV.namedLvar tmpName

  val bogusTy = CPSUtil.BOGt
  val uintKd  = P.UINT Target.mlValueSz
  val uintTy  = { sz=Target.mlValueSz, tag=false }
  val tintTy  = { sz=Target.defaultIntSz, tag=true }
  val uintCty = NUMt { sz=Target.mlValueSz, tag=false }

  fun getvar k =
    let val v = var ()
    in  LOOKER (P.GETVAR, [], v, bogusTy, k v)
    end

  fun num i = NUM { ival=IntInf.fromInt i, ty=uintTy }
  fun tnum i = NUM { ival=IntInf.fromInt i, ty=tintTy }

  fun arith (oper, args, v, exp) =
    PURE (P.PURE_ARITH { oper=oper, kind=uintKd }, args, v, uintCty, exp)

  fun idx (n, k) =
    let val v = var ()
    in  arith (P.MUL, [VAR n, num (Target.mlValueSz div 8)], v, k v)
    end

  fun increment (varptr, n, cexp) =
    let val curr = var ()
        val new = var ()
    in  idx (n, fn idx =>
          LOOKER (P.RAWLOAD { kind=uintKd }, [VAR varptr, VAR idx], curr, uintCty,
            arith (P.ADD, [VAR curr, num 1], new,
              SETTER (P.RAWSTORE { kind=uintKd }, [VAR varptr, VAR idx, VAR new], cexp))))
    end

  fun findIndexFrom (n, k) =
    let val v = var ()
    in  arith (P.RSHIFTL, [n, tnum (Target.mlValueSz - 3)], v, k v)
    end

  fun wrapFor (n, cexp) =
    findIndexFrom (n, fn idx =>
      getvar (fn var =>
        increment (var, idx, cexp)))

  fun instrument ((kind, name, args, tys, body): function) =
    let fun fix (f as (kind, name, args, tys, body)) =
              (kind, name, args, tys, exp body)
        and exp (RECORD (kind, fields, name, e)) =
              RECORD (kind, fields, name, exp e)
          | exp (SELECT (i, v, x, ty, e)) =
              SELECT (i, v, x, ty, exp e)
          | exp (OFFSET _) = raise Fail "no"
          | exp (APP (f, args)) = APP (f, args)
          | exp (FIX (functions, e)) =
              FIX (map fix functions, exp e)
          | exp (SWITCH (v, id, exps)) =
              SWITCH (v, id, map exp exps)
          | exp (BRANCH (br, args, id, e1, e2)) =
              BRANCH (br, args, id, exp e1, exp e2)
          | exp (SETTER (st, args, e)) =
              SETTER (st, args, exp e)
          | exp (LOOKER (lk, args, x, ty, e)) =
              LOOKER (lk, args, x, ty, exp e)
          | exp (ARITH (ar, args, x, ty, e)) =
              ARITH (ar, args, x, ty, exp e)
          | exp (PURE (p as P.WRAP (P.INT _ | P.UINT _), [n], x, ty, e)) =
              PURE (p, [n], x, ty, wrapFor (n, e))
          | exp (PURE (pr, args, x, ty, e)) =
              PURE (pr, args, x, ty, exp e)
          | exp (RCC (b, s, p, args, rets, e)) =
              RCC (b, s, p, args, rets, exp e)
    in  (kind, name, args, tys, exp body)
    end
end

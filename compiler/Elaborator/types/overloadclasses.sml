(* overloadclasses.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure OverloadClasses :> sig
  type class

  (* Predefined classes *)
  val intClass : class
  val wordClass : class
  val int_wordClass : class
  val realClass : class
  val int_realClass : class
  val numClass : class
  val textClass : class
  val num_textClass : class

  val inClass : Types.ty * class -> bool

  val members : class -> Types.ty list
  val default : class -> Types.ty

end = struct

    structure BT = BasicTypes

  (* overloadable types *)
  (* DEFAULT64: what about int31Ty? On 32-bit architectures, defaultIntTy would
   * be int31Ty. Since we no longer support 32-bit architecture, this may be OK.
   *)
    val intTys = [BT.int63Ty, BT.int32Ty, BT.int64Ty, BT.intinfTy]
    val wordTys = [BT.word63Ty, BT.word8Ty, BT.word32Ty, BT.word64Ty]
    val realTys = [BT.realTy]
    val textTys = [BT.charTy, BT.stringTy]

  (* overloading class *)
    type class = { members: Types.ty list, default: Types.ty }

(* overload classes *)
    val intClass = { members=intTys, default=BT.defaultIntTy }

    val wordClass = { members=wordTys, default=BT.defaultWordTy }

    val int_wordClass = { members=intTys @ wordTys, default=BT.defaultIntTy }

    val realClass = { members=realTys, default=BT.realTy }

    val	int_realClass = { members=intTys @ realTys, default=BT.defaultIntTy }

    val numClass = { members=intTys @ wordTys @ realTys, default=BT.defaultIntTy }

    val textClass = { members=textTys, default=BT.charTy } (* No operators belong to text class *)

    val num_textClass = { members = #members numClass @ #members textClass, default=BT.defaultIntTy }

    fun inClass (ty: Types.ty, { members, ... }: class) =
	List.exists (fn ty' => TypesUtil.equalType(ty', ty)) members

    fun members ({members, ...}: class) = members

    fun default ({default, ...}: class) = default

(* Note: realClass and textClass may be expanded in the future
*  when new overloaded operations over multiple real and text
*  types are added. *)

end (* structure OverloadClasses *)

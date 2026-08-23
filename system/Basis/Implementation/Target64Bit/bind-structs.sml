(* bind-structs.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Basis structure-alias bindings for 64-bit targets.  Common bindings
 * can be found in ../bind-structs.sml.
 *)

(* system word type *)
structure SysWordImp = Word64Imp

(* DEFAULT64: default numeric type *)
structure IntImp = Int64Imp
structure WordImp = Word64Imp

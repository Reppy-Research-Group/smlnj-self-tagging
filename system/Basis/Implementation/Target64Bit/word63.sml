(* word.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Default word structure (63 bits) for 64-bit targets.
 *)

structure Word63Imp : WORD =
  struct
    structure W63 = InlineT.Word63

    type word = Word63.word

    val wordSize = 63

    val toLarge   : word -> LargeWord.word = W63.toLarge
    val toLargeX  : word -> LargeWord.word = W63.toLargeX
    val fromLarge : LargeWord.word -> word = W63.fromLarge

  (* same as above, but deprecated *)
    val toLargeWord = toLarge
    val toLargeWordX = toLargeX
    val fromLargeWord = fromLarge

    val toLargeInt : word -> LargeInt.int = W63.toLargeInt
    val toLargeIntX  : word -> LargeInt.int = W63.toLargeIntX
    val fromLargeInt : LargeInt.int -> word = W63.fromLargeInt

    val toInt   : word -> int = W63.toInt
    val toIntX  : word -> int = W63.toIntX
    val fromInt : int -> word = W63.fromInt

    val orb  : word * word -> word = W63.orb
    val xorb : word * word -> word = W63.xorb
    val andb : word * word -> word = W63.andb
    val notb : word -> word = W63.notb

    val op * : word * word -> word = W63.*
    val op + : word * word -> word = W63.+
    val op - : word * word -> word = W63.-
    val op div : word * word -> word = W63.div
    val op mod : word * word -> word = W63.mod

    val <<  : word * Word.word -> word = W63.chkLshift
    val >>  : word * Word.word -> word = W63.chkRshiftl
    val ~>> : word * Word.word -> word = W63.chkRshift

    fun compare (w1, w2) =
	  if (W63.<(w1, w2)) then LESS
	  else if (W63.>(w1, w2)) then GREATER
	  else EQUAL
    val op > : word * word -> bool = W63.>
    val op >= : word * word -> bool = W63.>=
    val op < : word * word -> bool = W63.<
    val op <= : word * word -> bool = W63.<=

    val ~ : word -> word = ~
    val min : word * word -> word = W63.min
    val max : word * word -> word = W63.max

    fun fmt radix = (NumFormat64.fmtWord radix) o W63.toLarge
    val toString = fmt StringCvt.HEX

    fun scan radix = let
	  val scanLarge = NumScan64.scanWord radix
	  fun scan getc cs = (case (scanLarge getc cs)
		 of NONE => NONE
		  | (SOME(w, cs')) => if InlineT.Word64.>(w, 0wx7FFFFFFFFFFFFFFF)
		      then raise Overflow
		      else SOME(W63.fromLarge w, cs')
		(* end case *))
	  in
	    scan
	  end
    val fromString = PreBasis.scanString (scan StringCvt.HEX)

  (* added for Basis Library proposal 2026-001 *)
    val rotateL = W63.rotateL
    val rotateR = W63.rotateR

    val countZeros = W63.cntZeros
    val countOnes = W63.cntOnes

    val countLeadingZeros = W63.cntLeadingZeros
    val countLeadingOnes = W63.cntLeadingOnes

    val countTrailingZeros = W63.cntTrailingZeros
    val countTrailingOnes = W63.cntTrailingOnes

    val isPowerOf2 = W63.isPowOf2
    val ceilLog2 = W63.ceilLog2

  end  (* structure WordImp *)

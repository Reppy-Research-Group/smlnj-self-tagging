(* all.sml -- all sources for ray *)
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
(******************** ../BASIS/string.sig ********************)
(* string.sig
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

signature STRING =
  sig
    eqtype char
    eqtype string

    val maxSize : int
    val size      : string -> int

    val sub       : string * int -> char

    val str       : char -> string
    val extract   : string * int * int option -> string
    val substring : string * int * int -> string

    val ^         : string * string -> string
    val concat    : string list -> string
    val concatWith : string -> string list -> string

    val implode   : char list -> string
    val explode   : string -> char list
    val map       : (char -> char) -> string -> string
    val translate : (char -> string) -> string -> string
    val tokens    : (char -> bool) -> string -> string list
    val fields    : (char -> bool) -> string -> string list

    val isPrefix    : string -> string -> bool
    val isSubstring : string -> string -> bool
    val isSuffix    : string -> string -> bool

    val compare  : string * string -> order
    val collate  : (char * char -> order) -> string * string -> order

    val <  : (string * string) -> bool
    val <= : (string * string) -> bool
    val >  : (string * string) -> bool
    val >= : (string * string) -> bool

    val toString    : string -> String.string
    val scan        : (char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader
    val fromString  : String.string -> string option
    val toCString   : string -> String.string
    val fromCString : String.string -> string option

    (* Basis Library proposal 2015-003 *)
    val rev           : string -> string
    val implodeRev    : char list -> string

    val concatWithMap : string -> ('a -> string) -> 'a list -> string

  end
(******************** ../BASIS/string.sml ********************)
(* string.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure String : STRING =
  struct

    structure V = Unsafe.CharVector

  (* fast add/subtract avoiding the overflow test *)
    infix -- ++
    fun x -- y = Word.toIntX(Word.fromInt x - Word.fromInt y)
    fun x ++ y = Word.toIntX(Word.fromInt x + Word.fromInt y)

    fun uLessThan (x, y) = Word.<(Word.fromInt x, Word.fromInt y)

    val unsafeSub = V.sub
    val unsafeUpdate = V.update
    val unsafeCreate = V.create

  (* list reverse *)
    fun listRev ([], l) = l
      | listRev (x::r, l) = listRev (r, x::l)

    type char = char
    type string = string

    val maxSize = CharVector.maxLen

  (* these functions are implemented in base/system/smlnj/init/pervasive.sml *)
    val size = CharVector.length
    val op ^ = op ^
    val concat = concat
    val implode = implode
    val explode = explode
    val substring = substring

  (* allocate an uninitialized string of given length *)
    fun create n = if (uLessThan(maxSize, n))
	  then raise General.Size
	  else unsafeCreate n

    val chars = let
          fun next i = if (i <= 255)
                then let
                  val s = unsafeCreate 1
                  in
                    unsafeUpdate(s, 0, chr i);  s :: next(i+1)
                  end
                else []
          in
            Vector.fromList(next 0)
          end

    fun unsafeSubstring (_, _, 0) = ""
      | unsafeSubstring (s, i, 1) =
	  Vector.sub (chars, ord (unsafeSub (s, i)))
      | unsafeSubstring (s, i, n) = let
	  val ss = unsafeCreate n
	  fun copy j = if (j = n)
		then ()
		else (unsafeUpdate(ss, j, unsafeSub(s, i+j)); copy(j+1))
	  in
	    copy 0; ss
	  end

  (* concatenate a pair of non-empty strings *)
    fun concat2 (x, y) = let
	  val xl = size x and yl = size y
	  val ss = create(xl+yl)
	  fun copyx n = if (n = xl)
		then ()
		else (unsafeUpdate(ss, n, unsafeSub(x, n)); copyx(n+1))
	  fun copyy n = if (n = yl)
		then ()
		else (unsafeUpdate(ss, xl+n, unsafeSub(y,n)); copyy(n+1))
	  in
	    copyx 0; copyy 0;
	    ss
	  end

  (* given a reverse order list of strings and a total length, return
   * the concatenation of the list.
   *)
    fun revConcat (0, _) = ""
      | revConcat (1, lst) = let
	  fun find ("" :: r) = find r
	    | find (s :: _) = s
	    | find _ = "" (** impossible **)
	  in
	    find lst
	  end
      | revConcat (totLen, lst) = let
	  val ss = create totLen
	  fun copy ([], _) = ()
	    | copy (s::r, i) = let
		val len = size s
		val i = i - len
		fun copy' j = if (j = len)
		      then ()
		      else (
			unsafeUpdate(ss, i+j, unsafeSub(s, j));
			copy'(j+1))
		in
		  copy' 0;
		  copy (r, i)
		end
	  in
	    copy (lst, totLen);  ss
	  end

  (* added for Basis Library proposal 2015-003 *)
    fun implodeRev [] = ""
      | implodeRev l = let
	  fun length l = let
		fun loop (n, []) = n
		  | loop (n, [_]) = n ++ 1
		  | loop (n, _ :: _ :: l) = loop (n ++ 2, l)
		in
		  loop (0, l)
		end
	  val n = length l
	  val s = create n
	  fun fill ([], _) = s
	    | fill (c::cs, i) = (
		unsafeUpdate(s, i, c);
		fill (cs, i -- 1))
	  in
	    fill (l, n -- 1)
	  end
  (* end Basis Library proposal 2015-003 *)

  (* convert a character into a single character string *)
    fun str (c : Char.char) : string = Vector.sub(chars, ord c)

  (* get a character from a string *)
    val sub : (string * int) -> char = CharVector.sub

    fun extract (v, base, optLen) = let
	  val len = size v
	  fun newVec n = let
		val newV = unsafeCreate n
		fun fill i = if (i < n)
		      then (unsafeUpdate(newV, i, unsafeSub(v, base ++ i)); fill(i ++ 1))
		      else ()
		in
		  fill 0; newV
		end
	  in
	    case (base, optLen)
	     of (0, NONE) => v
	      | (_, SOME 0) => if ((base < 0) orelse (len < base))
		    then raise General.Subscript
		    else ""
	      | (_, NONE) => if ((base < 0) orelse (len < base))
		      then raise General.Subscript
		    else if (base = len)
		      then ""
		      else newVec (len - base)
	      | (_, SOME 1) =>
		  if ((base < 0) orelse (len < (base ++ 1)))
		    then raise General.Subscript
		    else str(unsafeSub(v, base))
	      | (_, SOME n) =>
		  if ((base < 0) orelse (n < 0) orelse (len < (base ++ n)))
		    then raise General.Subscript
		    else newVec n
	    (* end case *)
	  end

  (* concatenate a list of strings, using the given separator string *)
    fun concatWith _ [] = ""
      | concatWith _ [x] = x
      | concatWith sep (h :: t) =
	  concat (listRev (foldl (fn (x, l) => x :: sep :: l) [h] t, []))

    fun map f vec = (case (size vec)
	   of 0 => ""
	    | len => let
		val newVec = unsafeCreate len
		fun mapf i = if (i < len)
		      then (unsafeUpdate(newVec, i, f(unsafeSub(vec, i))); mapf(i+1))
		      else ()
		in
		  mapf 0; newVec
		end
	  (* end case *))

  (* map a translation function across the characters of a string *)
    fun translate tr s = let
	  val stop = size s
	  fun mkList (i, totLen, lst) = if (i < stop)
		then let val s' = tr (unsafeSub (s, i))
		  in
		    mkList (i+1, totLen + size s', s' :: lst)
		  end
		else revConcat (totLen, lst)
          in
	    mkList (0, 0, [])
          end

  (* tokenize a string using the given predicate to define the delimiter
   * characters.
   *)
    fun tokens isDelim s = let
	  val n = size s
	  fun substr (i, j, l) = if (i = j)
		then l
		else unsafeSubstring(s, i, j -- i)::l
	  fun scanTok (i, j, toks) = if (j < n)
		  then if (isDelim (unsafeSub (s, j)))
		    then skipSep(j ++ 1, substr(i, j, toks))
		    else scanTok (i, j ++ 1, toks)
		  else substr(i, j, toks)
	  and skipSep (j, toks) = if (j < n)
		  then if (isDelim (unsafeSub (s, j)))
		    then skipSep(j ++ 1, toks)
		    else scanTok(j, j ++ 1, toks)
		  else toks
	  in
	    listRev (scanTok (0, 0, []), [])
	  end
    fun fields isDelim s = let
	  val n = size s
	  fun substr (i, j, l) = unsafeSubstring(s, i, j -- i)::l
	  fun scanTok (i, j, toks) = if (j < n)
		  then if (isDelim (unsafeSub (s, j)))
		    then scanTok (j+1, j+1, substr(i, j, toks))
		    else scanTok (i, j+1, toks)
		  else substr(i, j, toks)
	  in
	    listRev (scanTok (0, 0, []), [])
	  end

  (* String comparisons *)
    local
      fun isPrefix' (s1, s2, i2, n2) = let
            val n1 = size s1
            fun eq (i, j) =
                  (i >= n1)
                  orelse ((unsafeSub(s1, i) = unsafeSub(s2, j)) andalso eq(i+1, j+1))
            in
              (n2 >= n1) andalso eq (0, i2)
            end
    in
    fun isPrefix s1 s2 = isPrefix' (s1, s2, 0, size s2)
    fun isSuffix s1 s2 = let
          val sz2 = size s2
          in
            isPrefix' (s1, s2, sz2 - size s1, sz2)
          end
    end

    (* Knuth-Morris-Pratt String Matching
     *
     * val kmp : string -> string * int * int -> int option
     * Find the first string within the second, starting at and
     * ending before the given positions.
     * Return the starting position of the match
     * or the given ending position if there is no match. *)
    fun kmp pattern = let
          val psz = size pattern
          val next = Array.array (psz, ~1)
          fun pat x = unsafeSub (pattern, x)
          fun nxt x = Array.sub (next, x)
          fun setnxt (i, x) = Array.update (next, i, x)
          (* trying to fill next at position p (> 0) and higher;
           * invariants: x >= 0
           *             pattern[0..x) = pattern[p-x..p)
           *             for i in [0..p) :
           *                 pattern[i] <> pattern[next[i]]
           *                 pattern[0..next[i]) = pattern[i-next[i]..i) *)
          fun fill (p, x) = if p >= psz then ()
                            else if pat x = pat p then dnxt (p, nxt x, x + 1)
                            else dnxt (p, x, nxt x + 1)
          and dnxt (p, x, y) = (setnxt (p, x); fill (p + 1, y))
          (* Once next has been initialized, it serves the following purpose:
           * Suppose we are looking at text position t and pattern position
           * p.  This means that all pattern positions < p have already
           * matched the text positions that directly precede t.
           * Now, if the text[t] matches pattern[p], then we simply advance
           * both t and p and try again.
           * However, if the two do not match, then we simply
           * try t again, but this time with the pattern position
           * given by next[p].
           * Success is when p reaches the end of the pattern, failure is
           * when t reaches the end of the text without p having reached the
           * end of the pattern. *)
          fun search (text, start, tsz) = let
                fun txt x = unsafeSub (text, x)
                fun loop (p, t) =
                    if p >= psz then t - psz
                    else if t >= tsz then tsz
                    else if p < 0 orelse pat p = txt t then loop (p+1, t+1)
                    else loop (nxt p, t)
                in
                  loop (0, start)
                end
          in
            fill (1, 0); search
          end

    fun isSubstring s = let
          val stringsearch = kmp s
          fun search s' = let
              val epos = size s'
              in
                stringsearch (s', 0, epos) < epos
              end
          in
            search
          end

    fun collate cmpFn (s1, s2) = let
          val n1 = size s1
          val n2 = size s2
	  val (n, order) =
		if (n1 = n2) then (n1, EQUAL)
		else if (n1 < n2) then (n1, LESS)
		else (n2, GREATER)
	  fun cmp i = if (i = n)
		then order
		else let
		  val c1 = unsafeSub(s1, i)
		  val c2 = unsafeSub(s2, i)
		  in
		    case (cmpFn(c1, c2))
		     of EQUAL => cmp (i+1)
		      | order => order
		    (* end case *)
		  end
          in
            cmp 0
          end

    fun compare (a, b) = let
	  fun cmpFn (c1, c2) =
		if (c1 = c2) then EQUAL
		else if (Char.>(c1, c2)) then GREATER
		else LESS
          in
            collate cmpFn (a, b)
          end

    val scan = String.scan
    val fromString = StringCvt.scanString scan
    val toString = translate Char.toString
    val fromCString = String.fromCString
    val toCString = translate Char.toCString

  (* added for Basis Library proposal 2015-003 *)
    fun rev s = let
	  val n = size s
	  in
	    if (n < 2)
	      then s
	      else let
		val s' = unsafeCreate n
		fun fill i = if (i < n)
		      then (unsafeUpdate(s', i, unsafeSub(s, n--i--1)); fill(i++1))
		      else ()
		in
		  fill 0; s'
		end
	  end

    fun concatWithMap sep cvtFn = let
	  fun concat' [] = ""
	    | concat' [x] = cvtFn x
	    | concat' (x::xs) = let
		val sepLen = size sep
		fun cvt ([], strs, len) = let
		      val s' = unsafeCreate len
		      fun fill ([], _) = s'
			| fill (s::ss, i) = let
			    val n = size s
			    val i = i -- n
			    fun copy j = if j < n
				  then (unsafeUpdate(s', i++j, unsafeSub(s, j)); copy(j++1))
				  else fill (ss, i)
			    in
			      copy 0
			    end
		      in
			fill (strs, len)
		      end
		  | cvt (x::xs, strs, len) = let
		      val s = cvtFn x
		      val len = len ++ sepLen ++ size s
		      in
			if len > maxSize then raise General.Size else ();
			cvt (xs, s::sep::strs, len)
		      end
		val s = cvtFn x
		in
		  cvt (xs, [s], size s)
		end
	  in
	    concat'
	  end
  (* end Basis Library proposal 2015-003 *)

  (* String greater or equal *)
    fun sgtr (a, b) = let
	  val al = size a and bl = size b
	  val n = if (al < bl) then al else bl
	  fun cmp i = if (i = n)
		then (al > bl)
		else let
		  val ai = unsafeSub(a,i)
		  val bi = unsafeSub(b,i)
		  in
		    Char.>(ai, bi) orelse ((ai = bi) andalso cmp(i+1))
		  end
	  in
	    cmp 0
	  end

    fun op <= (a,b) = if sgtr(a,b) then false else true
    fun op < (a,b) = sgtr(b,a)
    fun op >= (a,b) = b <= a
    val op > = sgtr

  end

(* pervasive string functions *)
val concat = String.concat
val op ^ = String.^
val str = String.str
val size = String.size
val implode = String.implode
val explode = String.explode
val substring = String.substring
(******************** objects.sml ********************)
(* objects.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Type declarations for the various objects in the ray tracer.
 *)

structure Objects =
  struct

    datatype point = PT of {x : real, y : real, z : real}

    datatype vector = VEC of {l : real, m : real, n : real}

    datatype ray = Ray of {s : point, d : vector}

    datatype camera = Camera of {
        vp : point,
        ul : point,
        ur : point,
        ll : point,
        lr : point
      }

    datatype color = Color of {red : real, grn : real, blu : real}

    datatype sphere = Sphere of {c : point, r : real, color : color}

    datatype hit = Miss | Hit of {t : real, s : sphere}

    datatype visible = Visible of {h : point, s : sphere}

    datatype object
      = TOP
      | NUMBER of real
      | NAME of string
      | LIST of object list
      | OPERATOR of object list -> object list
      | MARK
      | LITERAL of string
      | UNMARK
      | POINT of point
      | VECTOR of vector
      | RAY of ray
      | CAMERA of camera
      | COLOR of color
      | SPHERE of sphere
      | HIT
      | VISIBLE

  end (* Objects *)
(******************** interp.sml ********************)
(* interp.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Interp : sig

    exception Stop

    val installOperator : string * (Objects.object list -> Objects.object list) -> unit

    val parse : TextIO.instream -> unit

    val error : string -> Objects.object list -> 'a

  end = struct

    local
      val exit = OS.Process.exit
      fun ordof(s, i) = Char.ord(String.sub(s, i))
      exception NotAChar
      exception NotAReal
      fun fromStr x =
        (case Char.fromString x
          of SOME c => c
           | NONE => raise NotAChar)

      fun strToReal s =
       (case Real.fromString s
         of SOME r => r
         | _ => raise NotAReal)

      fun intToReal x = (strToReal ((Int.toString x) ^ ".0"))

      val explode = (fn x => map Char.toString (explode x))
      val implode = (fn x => implode (map fromStr x))

      open Objects
      val dict = ref ([] : {key : string, value : object} list)
      fun dictInsert (NAME key, value) = let
            fun find [] = [{key=key, value=value}]
              | find (x::r) = if (key = #key x)
                  then {key=key, value=value}::r
                  else x :: (find r)
            in
              dict := find(!dict)
            end
        | dictInsert _ = raise Fail "dictInsert"
      fun prObj outStrm obj = let
            fun printf args = Log.error args
            fun pr (NUMBER n) = printf["  ", Real.toString n, "\n"]
              | pr (NAME s) = printf["  ",  s, "\n"]
              | pr (LITERAL s) = printf["  ", s, "\n"]
              | pr (LIST l) = app pr l
              | pr MARK = printf["  MARK\n"]
              | pr (OPERATOR _) = printf["  <operator>\n"]
              | pr TOP = printf["  TOP OF STACK\n"]
              | pr _ = printf["  <object>\n"]
            in
              pr obj
            end
    in

    exception Stop

    fun error opName stk = let
          fun prStk ([], _) = ()
            | prStk (_, 0) = ()
            | prStk (obj::r, i) = (prObj TextIO.stdErr obj; prStk(r, i-1))
          in
            Log.error ["ERROR: ", opName, "\n"];
            prStk (stk, 10);
            raise (Fail opName)
          end

    fun installOperator (name, rator) =
          dictInsert (NAME name, OPERATOR rator)

    fun ps_def (v::k::r) = (dictInsert(k, v); r)
      | ps_def stk = error "ps_def" stk

    local
      fun binOp (f, opName) = let
            fun g ((NUMBER arg1)::(NUMBER arg2)::r) =
                  NUMBER(f(arg2, arg1)) :: r
              | g stk = error opName stk
            in
              g
            end
    in
    val ps_add = binOp (op +, "add")
    val ps_sub = binOp (op -, "sub")
    val ps_mul = binOp (op *, "mul")
    val ps_div = binOp (op /, "div")
    end

    fun ps_rand stk = (NUMBER 0.5)::stk (** ??? **)

    fun ps_print (obj::r) = (prObj TextIO.stdOut obj; r)
      | ps_print stk = error "print" stk

    fun ps_dup (obj::r) = (obj::obj::r)
      | ps_dup stk = error "dup" stk

    fun ps_stop _ = raise Stop

  (* initialize dictionary and begin parsing input *)
    fun parse inStrm = let
          fun getc () = case TextIO.input1 inStrm of NONE => ""
                               | SOME c => Char.toString c
          fun peek () = case TextIO.lookahead inStrm
                         of SOME x => Char.toString x
                          | _ => ""
        (* parse one token from inStrm *)
          fun toke deferred = let
                fun doChar "" = exit OS.Process.success
                  | doChar "%" = let
                      fun lp "\\n" = doChar(getc())
                        | lp "" = exit OS.Process.success
                        | lp _ = lp(getc())
                      in
                        lp(getc())
                      end
                  | doChar "{" = (MARK, deferred+1)
                  | doChar "}" = (UNMARK, deferred-1)
                  | doChar c = if Char.isSpace (fromStr c)
                      then doChar(getc())
                      else let
                        fun lp buf = (case peek()
                               of "{" => buf
                                | "}" => buf
                                | "%" => buf
                                | c => if Char.isSpace(fromStr c)
                                    then buf
                                    else (getc(); lp(c::buf))
                              (* end case *))
                        val tok = implode (rev (lp [c]))
                        val hd = ordof(tok, 0)
                        in
                          if (hd = ord (#"/"))
                            then (LITERAL(substring(tok, 1, size tok - 1)), deferred)
                          else
                            if ((Char.isDigit (chr hd)) orelse (hd = ord (#"-")))
                            then (NUMBER(strToReal(tok)), deferred)
                            else (NAME tok, deferred)
                        end
                in
                  doChar(getc())
                end
        (* execute a token (if not deferred) *)
          fun exec (UNMARK, stk, _) = let
                fun lp ([], _) = raise Fail "MARK"
                  | lp (MARK::r, l) = (LIST l)::r
                  | lp (x::r, l) = lp (r, x::l)
                  in
                    lp (stk, [])
                  end
            | exec (OPERATOR f, stk, 0) = f stk
            | exec (LIST l, stk, 0) = let
                fun execBody ([], stk) = stk
                  | execBody (obj::r, stk) = (exec(obj, stk, 0); execBody(r, stk))
                in
                  execBody (l, stk)
                end
            | exec (NAME s, stk, 0) = let
                fun find [] = raise Fail "undefined name"
                  | find ({key, value}::r) = if (key = s) then value else find r
                in
                  exec (find (!dict), stk, 0)
                end
            | exec (obj, stk, _) = obj::stk
          fun lp (stk, level) = let
                val (obj, level) = toke level
                val stk = exec (obj, stk, level)
                in
                  lp (stk, level)
                end
          in
            installOperator ("add", ps_add);
            installOperator ("def", ps_def);
            installOperator ("div", ps_div);
            installOperator ("dup", ps_dup);
            installOperator ("mul", ps_mul);
            installOperator ("print", ps_print);
            installOperator ("rand", ps_rand);
            installOperator ("stop", ps_stop);
            installOperator ("sub", ps_sub);
            (lp ([], 0)) handle Stop => ()
          end (* parse *)

    end (* local *)

  end (* Interp *)
(******************** ray.sml ********************)
(* ray.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Ray =
  struct
    local open Objects in

  (** basic operations on points and vectors **)

    fun scaleVector (s, VEC{l, m, n}) = VEC{l=s*l, m=s*m, n=s*n}

    fun vecPlusVec (VEC{l, m, n}, VEC{l=l', m=m', n=n'}) = VEC{l=l+l', m=m+m', n=n+n'}

    fun vecPlusPt (VEC{l, m, n}, PT{x, y, z}) = PT{x=x+l, y=y+m, z=z+n}

    fun ptMinusPt (PT{x, y, z}, PT{x=x', y=y', z=z'}) = VEC{l=x-x', m=y-y', n=z-z'}

    fun wave (PT{x, y, z}, PT{x=x', y=y', z=z'}, w) = PT{
            x = w * (x' - x) + x,
            y = w * (y' - y) + y,
            z = w * (z' - z) + z
          }

    fun dotProd (VEC{l, m, n}, VEC{l=l', m=m', n=n'}) = ((l*l') + (m*m') + (n*n'))

  (* normal vector to sphere *)
    fun normalSphere (Visible{h, s as Sphere{c, ...}}) = let
          val n = ptMinusPt(h, c)
          val norm = Math.sqrt(dotProd(n, n))
          in
            scaleVector(1.0 / norm, n)
          end

  (* intersect a ray with a sphere *)
    fun intersectSphere (Ray ray, s as Sphere sphere) = let
          val a = dotProd(#d ray, #d ray)
          val sdiffc = ptMinusPt(#s ray, #c sphere)
          val b = 2.0 * dotProd(sdiffc, #d ray)
          val c = dotProd(sdiffc, sdiffc) - (#r sphere * #r sphere)
          val d = b*b - 4.0*a*c
          in
            if (d <= 0.0)
              then Miss
              else let
                val d = Math.sqrt(d)
                val t1 = (~b - d) / (2.0 * a)
                val t2 = (~b + d) / (2.0 * a)
                val t = if ((t1 > 0.0) andalso (t1 < t2)) then t1 else t2
                in
                  Hit{t=t, s=s}
                end
          end

  (* simple shading function *)
    fun shade {light, phi} (visible as Visible{h, s}) = let
          val l = ptMinusPt(light, h)
          val n = normalSphere(visible)
          val irradiance = phi * dotProd(l,n) / dotProd(l,l);
          val irradiance = (if (irradiance < 0.0) then 0.0 else irradiance) + 0.05
          val Sphere{color=Color{red, grn, blu}, ...} = s
          in
            Color{red=red*irradiance, grn=grn*irradiance, blu=blu*irradiance}
          end

    fun trace (ray as (Ray ray'), objList) = let
          fun closest (Miss, x) = x
            | closest (x, Miss) = x
            | closest (h1 as Hit{t=t1, ...}, h2 as Hit{t=t2, ...}) =
                if (t2 < t1) then h2 else h1
          fun lp ([], Hit{t, s}) = Visible{
                  h = vecPlusPt(scaleVector(t, #d ray'), #s ray'),
                  s = s
                }
            | lp (s :: r, closestHit) =
                lp (r, closest (closestHit, intersectSphere (ray, s)))
            | lp _ = raise Fail "trace"
          in
            lp (objList, Miss)
          end

    fun camera (Camera cam) (x, y) = let
          val l = wave (#ul cam, #ll cam, y)
          val r = wave (#ur cam, #lr cam, y)
          val image_point = wave(l, r, x)
          in
            Ray{d = ptMinusPt(image_point, #vp cam), s = #vp cam}
          end

    val shade = shade {light = PT{x = 10.0, y = ~10.0, z = ~10.0}, phi = 16.0}
    val camera = camera (Camera{
            vp = PT{x = 0.0, y = 0.0, z = ~3.0},
            ul = PT{x = ~1.0, y = ~1.0, z = 0.0},
            ur = PT{x = 1.0, y = ~1.0, z = 0.0},
            ll = PT{x = ~1.0, y = 1.0, z = 0.0},
            lr = PT{x = 1.0, y = 1.0, z = 0.0}
          })

    fun image objList (x, y) = shade (trace(camera(x, y), objList))

    structure BinIO = Log.BinIO

(* TODO: switch to PPM output *)
    fun picture (picName, objList) = let
          val outStrm = BinIO.openOut picName
          val image = image objList
          fun put b = BinIO.output1 (outStrm, b)
          fun doPixel (i, j) = let
                val x = (real i) / 512.0
                val y = (real j) / 512.0
                val (Color c) = image (x, y)
                fun cvt x = if (x >= 1.0) then 0w255 else Word8.fromInt(floor(256.0*x))
                in
                  put (cvt (#red c));
                  put (cvt (#grn c));
                  put (cvt (#blu c))
                end
          fun lp_j j = if (j < 512)
                then let
                  fun lp_i i = if (i < 512)
                        then (doPixel(i, j); lp_i(i+1))
                        else ()
                  in
                    lp_i 0; lp_j(j+1)
                  end
                else ()
          in
            BinIO.output (outStrm, Byte.stringToBytes (concat [
                "P6\n512 512\n255\n"
              ]));
            lp_j 0;
            BinIO.closeOut outStrm
          end

    end (* local *)
  end (* Ray *)
(******************** interface.sml ********************)
(* interface.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * The interface between the interpreter and the ray tracer.
 *)

structure Interface : sig

    val rtInit : unit -> unit

  end = struct

    open Objects

  (* color pops three numbers and pushes a color object.
   * usage: red-value green-value blue-value color
   *)
    fun ps_color ((NUMBER blu)::(NUMBER grn)::(NUMBER red)::r) =
          (COLOR(Color{red=red, grn=grn, blu=blu})) :: r
      | ps_color stk = Interp.error "color" stk

  (* pop radius, coordinates of center, and a color and push a sphere
   * usage: radius x y z color-value sphere
   *)
    fun ps_sphere (
          (COLOR c)::(NUMBER z)::(NUMBER y)::(NUMBER x)::(NUMBER rad)::r
        ) = SPHERE(Sphere{c=PT{x=x, y=y, z=z}, r=rad, color=c}) :: r
      | ps_sphere stk = Interp.error "sphere" stk

  (* build an object list from solids on the stack, then invoke raytracer *)
    fun ps_raytrace ((LITERAL picName)::r) = let
          fun mkObjList ([], l) = l
            | mkObjList ((SPHERE s)::r, l) = mkObjList(r, s::l)
            | mkObjList (_::r, l) = mkObjList(r, l)
          in
            Ray.picture(picName, mkObjList(r, []));
            []
          end
      | ps_raytrace stk = Interp.error "raytrace" stk

  (* add ray tracing operations to interpreter dictionary *)
    fun rtInit () = (
          Interp.installOperator("color", ps_color);
          Interp.installOperator("sphere", ps_sphere);
          Interp.installOperator("raytrace", ps_raytrace))

  end
in
(******************** main.sml ********************)
(* main.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Main structure for running the benchmark.
 *)

structure Main : BMARK =
  struct

    val name = "ray"

    val results = ["out.ppm"]

    fun trace file = let
	  val strm = TextIO.openIn file
	  in
	    Interface.rtInit ();
	    Interp.parse strm;
	    TextIO.closeIn strm
	  end

    fun doit () = let
          fun loop n = if n = 0
                then ()
                else (trace "DATA/bmark.txt"; loop (n - 1))
          in
            loop 500
          end

    fun testit _ = trace "DATA/test.txt"

  end
end

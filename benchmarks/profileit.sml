structure Profiling : sig
  val profile : TextIO.outstream * (unit -> 'a) -> unit
  val profileUse : TextIO.outstream * string -> unit
end = struct
  structure CI = Unsafe.CInterface

  val clear : int -> unit      = CI.c_function "SMLNJ-RunT" "profCounterClear"
  val read  : unit -> int list = CI.c_function "SMLNJ-RunT" "profCounterRead"

  fun profile (outstrm, doit) =
    let val () = clear 8
        val _  = doit ()
    in  case read ()
          of [x000, x001, x010, x011, x100, x101, x110, x111] =>
               TextIO.output (outstrm, concat [
                 "0x000:\t", Int.toString x000, "\n",
                 "0x001:\t", Int.toString x001, "\n",
                 "0x010:\t", Int.toString x010, "\n",
                 "0x011:\t", Int.toString x011, "\n",
                 "0x100:\t", Int.toString x100, "\n",
                 "0x101:\t", Int.toString x101, "\n",
                 "0x110:\t", Int.toString x110, "\n",
                 "0x111:\t", Int.toString x111, "\n"
               ])
           | _ => raise Fail "impossible"
    end

  fun profileUse (outstrm, filename) =
    profile (outstrm, fn () => (use filename; ()))
end


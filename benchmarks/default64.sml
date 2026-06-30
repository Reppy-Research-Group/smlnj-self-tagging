local

overload abs : ('a -> 'a)
  as Int64.abs and Int32.abs and Int.abs and IntInf.abs and Real.abs
overload + :   ('a * 'a -> 'a)
  as  Int64.+ and Int32.+ and Int.+ and IntInf.+
  and Word.+ and Word8.+ and Word32.+ and Word64.+
  and Real64.+
overload - :   ('a * 'a -> 'a)
  as  Int64.- and Int32.- and Int.- and IntInf.-
  and Word.- and Word8.- and Word32.- and Word64.-
  and Real64.-
overload * :   ('a * 'a -> 'a)
  as  Int64.* and Int32.* and Int.* and IntInf.*
  and Word.* and Word8.* and Word32.* and Word64.*
  and Real64.*
overload < :   ('a * 'a -> bool)
  as  Int64.< and Int32.< and Int.< and IntInf.<
  and Word.< and Word8.< and Word32.< and Word64.<
  and Real.<
  and Char.<
  and String.<
overload <= :   ('a * 'a -> bool)
  as  Int64.<= and Int32.<= and Int.<= and IntInf.<=
  and Word.<= and Word8.<= and Word32.<= and Word64.<=
  and Real.<=
  and Char.<=
  and String.<=
overload > :   ('a * 'a -> bool)
  as  Int64.> and Int32.> and Int.> and IntInf.>
  and Word.> and Word8.> and Word32.> and Word64.>
  and Real.>
  and Char.>
  and String.>
overload >= :   ('a * 'a -> bool)
  as  Int64.>= and Int32.>= and Int.>= and IntInf.>=
  and Word.>= and Word8.>= and Word32.>= and Word64.>=
  and Real.>=
  and Char.>=
  and String.>=
overload div : ('a * 'a -> 'a)
  as  Int64.div and Int32.div and Int.div and IntInf.div
  and Word.div and Word8.div and Word32.div and Word64.div
overload mod : ('a * 'a -> 'a)
  as  Int64.mod and Int32.mod and Int.mod and IntInf.mod
  and Word.mod and Word8.mod and Word32.mod and Word64.mod
overload abs : ('a -> 'a)
  as Int64.abs and Int32.abs and Int.abs and IntInf.abs and Real.abs

type int = Int64.int
structure Int = Int64
val real = Real.fromInt o Int64.toInt
val int = Int64.toInt

structure Word = struct
  open Word
  val toInt = fn (w: word) => Int64.fromInt (toInt w)
  val toIntX = fn (w: word) => Int64.fromInt (toIntX w)
  val fromInt = fn (i: int) => fromInt (Int64.toInt i)
end

structure Real64 = struct
  open Real
  val fromInt = fn (i: int) => fromInt (Int64.toInt i)
end

in


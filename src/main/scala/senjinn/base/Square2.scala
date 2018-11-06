package senjinn.base

import enumeratum._


sealed abstract class Square2(val index: Int) extends EnumEntry
{
  val (loc, rank, file) = (1L << index, index / 8, index % 8)
  
  
}

object Square2 extends Enum[Square2]
{
  val values = findValues
  
  // First rank
  case object h1 extends Square2(0)
  case object g1 extends Square2(1)
  case object f1 extends Square2(2)
  case object e1 extends Square2(3)
  case object d1 extends Square2(4)
  case object c1 extends Square2(5)
  case object b1 extends Square2(6)
  case object a1 extends Square2(7)
  
  // Second rank
  case object h2 extends Square2(8)
  case object g2 extends Square2(9)
  case object f2 extends Square2(10)
  case object e2 extends Square2(11)
  case object d2 extends Square2(12)
  case object c2 extends Square2(13)
  case object b2 extends Square2(14)
  case object a2 extends Square2(15)
  
  // Third rank
  case object h3 extends Square2(16)
  case object g3 extends Square2(17)
  case object f3 extends Square2(18)
  case object e3 extends Square2(19)
  case object d3 extends Square2(20)
  case object c3 extends Square2(21)
  case object b3 extends Square2(22)
  case object a3 extends Square2(23)
  
  // Fourth rank
  case object h4 extends Square2(24)
  case object g4 extends Square2(25)
  case object f4 extends Square2(26)
  case object e4 extends Square2(27)
  case object d4 extends Square2(28)
  case object c4 extends Square2(29)
  case object b4 extends Square2(30)
  case object a4 extends Square2(31)
  
  // Fifth rank
  case object h5 extends Square2(32)
  case object g5 extends Square2(33)
  case object f5 extends Square2(34)
  case object e5 extends Square2(35)
  case object d5 extends Square2(36)
  case object c5 extends Square2(37)
  case object b5 extends Square2(38)
  case object a5 extends Square2(39)
  
  // Sixth rank
  case object h6 extends Square2(40)
  case object g6 extends Square2(41)
  case object f6 extends Square2(42)
  case object e6 extends Square2(43)
  case object d6 extends Square2(44)
  case object c6 extends Square2(45)
  case object b6 extends Square2(46)
  case object a6 extends Square2(47)
  
  // Seventh rank
  case object h7 extends Square2(48)
  case object g7 extends Square2(49)
  case object f7 extends Square2(50)
  case object e7 extends Square2(51)
  case object d7 extends Square2(52)
  case object c7 extends Square2(53)
  case object b7 extends Square2(54)
  case object a7 extends Square2(55)
  
  // Eighth rank
  case object h8 extends Square2(56)
  case object g8 extends Square2(57)
  case object f8 extends Square2(58)
  case object e8 extends Square2(59)
  case object d8 extends Square2(60)
  case object c8 extends Square2(61)
  case object b8 extends Square2(62)
  case object a8 extends Square2(63)
}

object Main extends App
{
  println(Square2.h1)
}
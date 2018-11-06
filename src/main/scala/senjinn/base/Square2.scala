package senjinn.base

import enumeratum._


sealed abstract class Square2(val index: Int) extends EnumEntry
{
  val (loc, rank, file) = (1L << index, index / 8, index % 8)
  
  
}

object Square2 extends Enum[Square2]
{
  val values = findValues
  
  case object h1 extends Square2(0)
  case object g1 extends Square2(1)
  case object f1 extends Square2(2)
}

object Main extends App
{
  println(Square2.h1)
}
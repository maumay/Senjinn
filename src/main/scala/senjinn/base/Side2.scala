package senjinn.base

import enumeratum._


sealed abstract class Side2(val isWhite: Boolean) extends EnumEntry
{
  val penultimatePawnRank = if (isWhite) 6 else 1
}

object Side2 extends Enum[Side2]
{
  def other(side: Side2): Side2 = side match {
    case White => Black
    case Black => White
  }
  
  val values = findValues
  
  case object White extends Side2(true)
  case object Black extends Side2(false)
}
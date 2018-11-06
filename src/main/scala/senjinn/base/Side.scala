package senjinn.base

import enumeratum._


sealed abstract class Side(val isWhite: Boolean) extends EnumEntry
{
  val penultimatePawnRank = if (isWhite) 6 else 1
}

object Side extends Enum[Side]
{
  def other(side: Side): Side = side match {
    case White => Black
    case Black => White
  }
  
  val values = findValues.toVector
  
  case object White extends Side(true)
  case object Black extends Side(false)
}
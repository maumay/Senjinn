package senjinn.base

import enumeratum._

sealed abstract class DevPiece2(val startSquare: Square2) extends EnumEntry
{
}

object DevPiece2 extends Enum[DevPiece2]
{
  val all = findValues.toVector
  
  private val startSquareMap = all.map(p => (p.startSquare, p)).toMap
  
  def apply(startSquare: Square2): DevPiece2 = {
    startSquareMap(startSquare)
  }
  
  import Square2._
  case object WhiteKingKnight  extends DevPiece2(g1)
  case object WhiteKingBishop  extends DevPiece2(f1)
  case object WhiteKingPawn    extends DevPiece2(e2)
  case object WhiteQueenPawn   extends DevPiece2(d2)
  case object WhiteQueenBishop extends DevPiece2(c1)
  case object WhiteQueenKnight extends DevPiece2(b1)
  
  case object BlackKingKnight  extends DevPiece2(g8)
  case object BlackKingBishop  extends DevPiece2(f8)
  case object BlackKingPawn    extends DevPiece2(e7)
  case object BlackQueenPawn   extends DevPiece2(d7)
  case object BlackQueenBishop extends DevPiece2(c8)
  case object BlackQueenKnight extends DevPiece2(b8)
}
package senjinn.base

import enumeratum._

sealed abstract class DevPiece(val startSquare: Square) extends EnumEntry
{
}

object DevPiece extends Enum[DevPiece]
{
  val values = findValues.toVector
  
  private val startSquareMap = values.map(p => (p.startSquare, p)).toMap
  
  def apply(startSquare: Square): Option[DevPiece] = {
    startSquareMap.get(startSquare)
  }
  
  import Square._
  case object WhiteKingKnight  extends DevPiece(g1)
  case object WhiteKingBishop  extends DevPiece(f1)
  case object WhiteKingPawn    extends DevPiece(e2)
  case object WhiteQueenPawn   extends DevPiece(d2)
  case object WhiteQueenBishop extends DevPiece(c1)
  case object WhiteQueenKnight extends DevPiece(b1)
  
  case object BlackKingKnight  extends DevPiece(g8)
  case object BlackKingBishop  extends DevPiece(f8)
  case object BlackKingPawn    extends DevPiece(e7)
  case object BlackQueenPawn   extends DevPiece(d7)
  case object BlackQueenBishop extends DevPiece(c8)
  case object BlackQueenKnight extends DevPiece(b8)
}
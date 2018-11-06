package senjinn.base

import enumeratum._
import senjinn.base.ImplicitAreaConverters.{boardsquare2squareset}


sealed abstract class CastleZone(val kingSource: Square, val kingTarget: Square) extends EnumEntry
{
  val (isWhiteZone, isKingsideZone) = (kingSource.rank == 0, kingTarget.file - kingSource.file < 0)
  
  val rookSource = if (isKingsideZone) kingSource >> 3 else kingSource << 4
  val rookTarget = if (isKingsideZone) kingTarget << 1 else kingTarget >> 1
  
  val requiredUncontrolled = if (isKingsideZone) {
    (0 to 2).map(i => (kingSource >> i): SquareSet).reduce(_|_)
  }
  else {
    (0 to 2).map(i => (kingSource << i): SquareSet).reduce(_|_)
  }
  
  val requiredClear = if (isKingsideZone) {
    (1 to 2).map(i => (kingSource >> i): SquareSet).reduce(_|_)
  }
  else {
    (1 to 3).map(i => (kingSource << i): SquareSet).reduce(_|_)
  }
}

object CastleZone extends Enum[CastleZone]
{
  def apply(identifier: String): CastleZone = {
    identifier.toLowerCase match {
      case x if x == "wk" => WhiteKing
      case x if x == "wq" => WhiteQueen
      case x if x == "bk" => BlackKing
      case x if x == "bq" => BlackQueen
      case _              => throw new RuntimeException(identifier)
    }
  }
  
  val values = findValues.toVector
  
  val emptySet: Set[CastleZone]      = Set[CastleZone]()
  val whiteKingSet: Set[CastleZone]  = Set(WhiteKing)
  val whiteQueenSet: Set[CastleZone] = Set(WhiteQueen)
  val whiteSet: Set[CastleZone]      = Set(WhiteKing, WhiteQueen)
  val blackKingSet: Set[CastleZone]  = Set(BlackKing)
  val blackQueenSet: Set[CastleZone] = Set(BlackQueen)
  val blackSet: Set[CastleZone]      = Set(BlackKing, BlackQueen)
  val completeSet: Set[CastleZone]   = values.to[Set]
  
  import Square.{e1, g1, c1, g8, e8, c8}
  case object WhiteKing  extends CastleZone(e1, g1)
  case object WhiteQueen extends CastleZone(e1, c1)
  case object BlackKing  extends CastleZone(e8, g8)
  case object BlackQueen extends CastleZone(e8, c8)
}

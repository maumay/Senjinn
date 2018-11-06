package senjinn.base

import enumeratum._
import senjinn.base.ImplicitAreaConverters.{boardsquare2squareset}


sealed abstract class CastleZone2(val kingSource: Square2, val kingTarget: Square2) extends EnumEntry
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

object CastleZone2 extends Enum[CastleZone2]
{
  def apply(identifier: String): CastleZone2 = {
    identifier.toLowerCase match {
      case x if x == "wk" => WhiteKing
      case x if x == "wq" => WhiteQueen
      case x if x == "bk" => BlackKing
      case x if x == "bq" => BlackQueen
      case _              => throw new RuntimeException(identifier)
    }
  }
  
  val emptySet      = Set[CastleZone2]()
  val whiteKingSet  = Set(WhiteKing)
  val whiteQueenSet = Set(WhiteQueen)
  val whiteSet      = Set(WhiteKing, WhiteQueen)
  val blackKingSet  = Set(BlackKing)
  val blackQueenSet = Set(BlackQueen)
  val blackSet      = Set(BlackKing, BlackQueen)
  val completeSet   = all.to[Set]
  
  val all = findValues.toVector
  
  import Square2.{e1, g1, c1, g8, e8, c8}
  case object WhiteKing  extends CastleZone2(e1, g1)
  case object WhiteQueen extends CastleZone2(e1, c1)
  case object BlackKing  extends CastleZone2(e8, g8)
  case object BlackQueen extends CastleZone2(e8, c8)
}

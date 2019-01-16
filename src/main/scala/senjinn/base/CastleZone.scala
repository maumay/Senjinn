package senjinn.base

import enumeratum._

/**
 * Represents the four areas on the board where the special action of castling
 * may take place.
 * @param kingSource the required location of the king prior to castling
 * in this area.
 * @param kingTarget the location of the king after castling in this area
 * has taken place.
 */
sealed abstract class CastleZone(val kingSource: Square, val kingTarget: Square) extends EnumEntry
{
  /** Denotes the player this area applies to. */
  val isWhiteZone = kingSource.rank == 0
  /** Denotes the side of the board this area resides in */
  val isKingsideZone = kingTarget.file - kingSource.file < 0
  
  /** Required location of appropriate rook prior to castling in this area. */
  val rookSource = if (isKingsideZone) kingSource >> 3 else kingSource << 4
  /** Location of appropriate rook after castling in this area. */
  val rookTarget = if (isKingsideZone) kingTarget << 1 else kingTarget >> 1
  
  /** 
   * Squares required to be free of enemy control before castling can take 
   * place in this area.
   */
  val requiredUncontrolled = if (isKingsideZone) {
    (0 to 2).map(i => (kingSource >> i): SquareSet).reduce(_|_)
  }
  else {
    (0 to 2).map(i => (kingSource << i): SquareSet).reduce(_|_)
  }
  
  /**
   * Squares required to be clear of any pieces before castling can take
   * place in this area.
   */
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

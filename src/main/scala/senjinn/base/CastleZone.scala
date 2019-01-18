package senjinn.base

import enumeratum._

/**
 * Represents the four areas on the board where the special action of castling may 
 * take place.
 * 
 * @param kingSource the required location of the king prior to castling in this area.
 * @param kingTarget the location of the king after castling in this area has taken 
 * place.
 */
sealed abstract class CastleZone(val kingSource: Square, val kingTarget: Square)
  extends EnumEntry {
  
  /** Denotes the player this area applies to. */
  val isWhiteZone = kingSource.rank == 0
  /** Denotes the side of the board this area resides in */
  val isKingsideZone = kingTarget.file - kingSource.file < 0

  /** Required location of appropriate rook prior to castling in this area. */
  val rookSource = if (isKingsideZone) kingSource >> 3 else kingSource << 4
  /** Location of appropriate rook after castling in this area. */
  val rookTarget = if (isKingsideZone) kingTarget << 1 else kingTarget >> 1

  /**
   * Squares required to be free of enemy control before castling can take place in 
   * this area.
   */
  val requiredUncontrolled = if (isKingsideZone) {
    (0 to 2).map(i => (kingSource >> i): SquareSet).reduce(_ | _)
  } else {
    (0 to 2).map(i => (kingSource << i): SquareSet).reduce(_ | _)
  }

  /**
   * Squares required to be clear of any pieces before castling can take place in 
   * this area.
   */
  val requiredClear = if (isKingsideZone) {
    (1 to 2).map(i => (kingSource >> i): SquareSet).reduce(_ | _)
  } else {
    (1 to 3).map(i => (kingSource << i): SquareSet).reduce(_ | _)
  }
}

/**
 * Defines the instances of the sealed class `[[CastleZone]]` as well as providing 
 * some useful related definitions.
 */
object CastleZone extends Enum[CastleZone] {
  /**
   * Retrieves an instance by it's abbreviated String name. More specifically:
   *
   *  - "wk" maps to WhiteKing
   *  - "wq" maps to WhiteQueen
   *  - "bk" maps to BlackKingside
   *  - "bq" maps to BlackQueenside
   *
   * The string inputs are case-insensitive.
   *
   * @param identifier the shorthand string denoting the required area.
   */
  def apply(identifier: String): CastleZone = {
    identifier.toLowerCase match {
      case x if x == "wk" => WhiteKing
      case x if x == "wq" => WhiteQueen
      case x if x == "bk" => BlackKing
      case x if x == "bq" => BlackQueen
      case _              => throw new RuntimeException(identifier)
    }
  }

  /** Vector containing all `[[CastleZone]]` instances. */
  val values = findValues.toVector

  /** Set of no `[[CastleZone]]` instances. */
  val emptySet: Set[CastleZone] = Set[CastleZone]()
  /** Singleton set containing the WhiteKing `[[CastleZone]]` instance. */
  val whiteKingSet: Set[CastleZone] = Set(WhiteKing)
  /** Singleton set containing the WhiteQueen `[[CastleZone]]` instance. */
  val whiteQueenSet: Set[CastleZone] = Set(WhiteQueen)
  /** Set containing the white `[[CastleZone]]` instances. */
  val whiteSet: Set[CastleZone] = Set(WhiteKing, WhiteQueen)
  /** Singleton set containing the BlackKingside `[[CastleZone]]` instance. */
  val blackKingSet: Set[CastleZone] = Set(BlackKing)
  /** Singleton set containing the BlackQueenside `[[CastleZone]]` instance. */
  val blackQueenSet: Set[CastleZone] = Set(BlackQueen)
  /** Set containing the black `[[CastleZone]]` instances. */
  val blackSet: Set[CastleZone] = Set(BlackKing, BlackQueen)
  /** Set containing all `[[CastleZone]]` instances. */
  val completeSet: Set[CastleZone] = values.to[Set]

  import Square.{ e1, g1, c1, g8, e8, c8 }
  /** Represents the white king side castling area */
  case object WhiteKing extends CastleZone(e1, g1)
  /** Represents the white queen side castling area */
  case object WhiteQueen extends CastleZone(e1, c1)
  /** Represents the black king side castling area */
  case object BlackKing extends CastleZone(e8, g8)
  /** Represents the black queen side castling area */
  case object BlackQueen extends CastleZone(e8, c8)
}

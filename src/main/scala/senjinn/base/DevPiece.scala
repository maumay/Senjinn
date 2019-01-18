package senjinn.base

import enumeratum._

/**
 * Represents a piece which should be moved with high priority in the opening of a
 * game.
 * @param startSquare the square this piece begins the game on.
 */
sealed abstract class DevPiece(val startSquare: Square) extends EnumEntry {
}

/**
 * Defines the instances of the sealed class `[[DevPiece]]` as well as providing some
 * useful related definitions.
 */
object DevPiece extends Enum[DevPiece] {
  
  /** Vector of all instances of `[[DevPiece]]` */
  val values = findValues.toVector

  private val startSquareMap = values.map(p => (p.startSquare, p)).toMap

  /**
   * Retrieves instances of `[[DevPiece]]` by their start square.
   */
  def apply(startSquare: Square): Option[DevPiece] = {
    startSquareMap.get(startSquare)
  }

  import Square._
  /** Represents the white king side knight. */
  case object WhiteKingKnight extends DevPiece(g1)
  /** Represents the white king side bishop. */
  case object WhiteKingBishop extends DevPiece(f1)
  /** Represents the pawn which starts on e2. */
  case object WhiteKingPawn extends DevPiece(e2)
  /** Represents the pawn which starts on d2. */
  case object WhiteQueenPawn extends DevPiece(d2)
  /** Represents the white queen side bishop. */
  case object WhiteQueenBishop extends DevPiece(c1)
  /** Represents the white queen side knight. */
  case object WhiteQueenKnight extends DevPiece(b1)

  /** Represents the black king side knight. */
  case object BlackKingKnight extends DevPiece(g8)
  /** Represents the black king side bishop. */
  case object BlackKingBishop extends DevPiece(f8)
  /** Represents the pawn which starts on e7. */
  case object BlackKingPawn extends DevPiece(e7)
  /** Represents the pawn which starts on d7. */
  case object BlackQueenPawn extends DevPiece(d7)
  /** Represents the black queen side bishop. */
  case object BlackQueenBishop extends DevPiece(c8)
  /** Represents the black queen side knight. */
  case object BlackQueenKnight extends DevPiece(b8)
}
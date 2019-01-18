package senjinn.base

import enumeratum._

/**
 * Represents the two possible sides in a chess game; white and black.
 */
sealed abstract class Side(val isWhite: Boolean) extends EnumEntry
{
  /** 
   * The index of the rank a pawn on this side must be on in order for it to get
   * promoted on it's next move.
   */
  val penultimatePawnRank = if (isWhite) 6 else 1
}

object Side extends Enum[Side]
{
  /**
   * Returns the opposing side to the given parameter.
   * @param side the color to find the enemy of.
   */
  def other(side: Side): Side = side match {
    case White => Black
    case Black => White
  }
  
  /** A vector containing both sides. */
  val values = findValues.toVector
  
  case object White extends Side(true)
  case object Black extends Side(false)
}
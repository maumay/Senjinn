package senjinn.board

import senjinn.base.{CastleZone, ChessPiece, DevPiece, Square}

/**
 * Essentially a struct which is used to keep track of information
 * lost when a move is used to evolve a state of play. It can be 
 * used (along with the same move) to undo the evolution.
 */
class MoveReverser
{
  var isConsumed = true
  var discardedCastleRights: Set[CastleZone] = Set()
  var pieceTaken: Option[ChessPiece] = None
  var pieceDeveloped: Option[DevPiece] = None
  var discardedEnpassant: Option[Square] = None
  var discardedHash: Long = 0L
  var discardedClockValue: Int = -1

  def clearCastleRights() {
    discardedCastleRights = CastleZone.emptySet
  }
}
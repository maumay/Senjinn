package senjinn.moves

import senjinn.base.{Square, CastleZone, DevPiece}
import senjinn.board.{Board, MoveReverser}

/**
 * Trait representing the general concept of a move
 * made by a player which evolves the state of some
 * state of play.
 */
trait Move
{
  private[moves] val source: Square
  private[moves] val target: Square
  private[moves] val rightsRemoved: Set[CastleZone]
  private[moves] val castleCommand: Option[CastleZone]
  private[moves] val pieceDeveloped: Option[DevPiece]

  def toCompactString: String
  private[moves] def updatePieceLocations(state: Board, reverser: MoveReverser): Unit
  private[moves] def revertPieceLocations(state: Board, reverser: MoveReverser): Unit

  final def makeMove(state: Board, reverser: MoveReverser) {
    assert(reverser.isConsumed)
    updateCastlingStatus(state, reverser)
    updatePieceLocations(state, reverser)
    updateDevelopedPieces(state, reverser)
    state.switchActive()
    reverser.discardedHash = state.hashCache.increment(state.computeHash)
    reverser.isConsumed = false
  }

  final def undoMove(state: Board, reverser: MoveReverser) {
    assert(!reverser.isConsumed)
    state.switchActive()
    revertCastlingStatus(state, reverser)
    revertPieceLocations(state, reverser)
    revertDevelopedPieces(state, reverser)
    state.clock = reverser.discardedClockValue
    state.enpassant = reverser.discardedEnpassant
    state.hashCache.decrement(reverser.discardedHash)
    reverser.isConsumed = true
  }

  final def makeMove(state: Board) {
    makeMove(state, new MoveReverser())
  }

  private def updateCastlingStatus(state: Board, reverser: MoveReverser) {
    castleCommand foreach {state.castleStatus.setStatus(_)}
    reverser.discardedCastleRights = rightsRemoved & state.castleStatus.rights
    state.castleStatus.rights --= reverser.discardedCastleRights
  }

  private def revertCastlingStatus(state: Board, reverser: MoveReverser) {
    castleCommand foreach {state.castleStatus.removeStatus(_)}
    state.castleStatus.rights ++= reverser.discardedCastleRights
  }

  private def updateDevelopedPieces(state: Board, reverser: MoveReverser) {
    reverser.pieceDeveloped = pieceDeveloped.filterNot(state.piecesDeveloped contains _)
    state.piecesDeveloped ++= reverser.pieceDeveloped
  }

  private def revertDevelopedPieces(state: Board, reverser: MoveReverser) {
    state.piecesDeveloped --= reverser.pieceDeveloped
  }
}

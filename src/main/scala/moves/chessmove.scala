package senjinn.moves

import senjinn.base.{Square, CastleZone, DevPiece}
import senjinn.board.{BoardState, MoveReverser}


trait ChessMove
{
  val source: Square
  val target: Square
  val rightsRemoved: Set[CastleZone]
  val castleCommand: Option[CastleZone]
  val pieceDeveloped: Option[DevPiece]

  def toCompactString: String
  def updatePieceLocations(state: BoardState, reverser: MoveReverser): Unit
  def revertPieceLocations(state: BoardState, reverser: MoveReverser): Unit

  final def makeMove(state: BoardState, reverser: MoveReverser) {
    assert(reverser.isConsumed)
    updateCastlingStatus(state, reverser)
    updatePieceLocations(state, reverser)
    updateDevelopedPieces(state, reverser)
    state.switchActive()
    reverser.discardedHash = state.hcache.increment(state.computeHash)
    reverser.isConsumed = false
  }

  final def undoMove(state: BoardState, reverser: MoveReverser) {
    assert(!reverser.isConsumed)
    state.switchActive()
    revertCastlingStatus(state, reverser)
    revertPieceLocations(state, reverser)
    revertDevelopedPieces(state, reverser)
    state.clock = reverser.discardedClockValue
    state.enpassant = reverser.discardedEnpassant
    state.hcache.decrement(reverser.discardedHash)
    reverser.isConsumed = true
  }

  def makeMove(state: BoardState) {
    makeMove(state, new MoveReverser())
  }

  private def updateCastlingStatus(state: BoardState, reverser: MoveReverser) {
    castleCommand foreach {state.cstatus.setStatus(_)}
    reverser.discardedCastleRights = rightsRemoved & state.cstatus.rights
    state.cstatus.rights --= reverser.discardedCastleRights
  }

  private def revertCastlingStatus(state: BoardState, reverser: MoveReverser) {
    castleCommand foreach {state.cstatus.removeStatus(_)}
    state.cstatus.rights ++= reverser.discardedCastleRights
  }

  private def updateDevelopedPieces(state: BoardState, reverser: MoveReverser) {
    reverser.pieceDeveloped = pieceDeveloped.filterNot(state.pdev contains _)
    state.pdev ++= reverser.pieceDeveloped
  }

  private def revertDevelopedPieces(state: BoardState, reverser: MoveReverser) {
    state.pdev --= reverser.pieceDeveloped
  }
}

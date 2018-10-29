package senjinn.moves

import senjinn.base.{Square, CastleZone}
import senjinn.board.{BoardState, MoveReverser}
import senjinn.pieces.{ChessPiece}

/**
 * Represents the act of enpassant in a chess game.
 */
final class EnpassantMove private[moves](val source: Square, val target: Square) extends ChessMove
{
  val enpassantSquare = if (source.rank < target.rank) target >> 8 else target << 8
      
  override val rightsRemoved = CastleZone.setOfNoZones
  override val castleCommand = None
  override val pieceDeveloped = None

  override def toCompactString = s"E$source$target"

  override def updatePieceLocations(state: BoardState, reverser: MoveReverser) {
    val activePawn = ChessPiece(state.active)(0)
    val passivePawn = ChessPiece(state.passive)(0)
    val plocs = state.plocs
    plocs.removeSquare(activePawn, source)
    plocs.addSquare(activePawn, target)
    plocs.removeSquare(passivePawn, enpassantSquare)
    reverser.pieceTaken = Some(passivePawn)
    reverser.discardedEnpassant = state.enpassant
    reverser.discardedClockValue = state.clock
    state.enpassant = None
    state.clock = 0
  }

  override def revertPieceLocations(state: BoardState, reverser: MoveReverser) {
    val activePawn = ChessPiece(state.active)(0)
    val passivePawn = ChessPiece(state.passive)(0)
    val plocs = state.plocs
    plocs.removeSquare(activePawn, target)
    plocs.addSquare(activePawn, source)
    plocs.addSquare(passivePawn, enpassantSquare)
  }
}

object EnpassantMove
{
  def apply(source: Square, target: Square) = {
    new EnpassantMove(source, target)
  }
}

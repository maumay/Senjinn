package senjinn.moves

import senjinn.base.{Square, CastleZone}
import senjinn.board.{BoardState, MoveReverser}
import senjinn.pieces.{ChessPiece}

class EnpassantMove private[moves](val source: Square, val target: Square) extends ChessMove
{
  val rightsRemoved = CastleZone.setOfNoZones
  val castleCommand = None
  val pieceDeveloped = None

  val enpassantSquare = if (source.rank < target.rank) target >> 8 else target << 8

  def toCompactString = s"E$source$target"

  def updatePieceLocations(state: BoardState, reverser: MoveReverser) {
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

  def revertPieceLocations(state: BoardState, reverser: MoveReverser) {
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

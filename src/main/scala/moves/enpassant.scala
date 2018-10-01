package senjinn.moves

import senjinn.base.{BoardSquare, CastleZone}
import senjinn.board.{BoardState, MoveReverser}
import senjinn.pieces.{ChessPiece}

class EnpassantMove(val source: BoardSquare, val target: BoardSquare) extends ChessMove
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
}

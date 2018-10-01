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
    
  }
}

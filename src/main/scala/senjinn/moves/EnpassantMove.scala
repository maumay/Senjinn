package senjinn.moves

import senjinn.base.{Square, CastleZone, Piece}
import senjinn.board.{Board, MoveReverser}

/**
 * Represents the act of enpassant in a chess game.
 */
final class EnpassantMove private[moves](val source: Square, val target: Square) extends ChessMove
{
  val enpassantSquare = if (source.rank < target.rank) target >> 8 else target << 8
  
  // ChessMove API
  override val rightsRemoved = CastleZone.emptySet
  override val castleCommand = None
  override val pieceDeveloped = None

  override def toCompactString = s"E$source$target"

  override def updatePieceLocations(state: Board, reverser: MoveReverser) {
    val activePawn = Piece(state.active)(0)
    val passivePawn = Piece(state.passive)(0)
    val plocs = state.pieceLocations
    plocs.removeSquare(activePawn, source)
    plocs.addSquare(activePawn, target)
    plocs.removeSquare(passivePawn, enpassantSquare)
    reverser.pieceTaken = Some(passivePawn)
    reverser.discardedEnpassant = state.enpassant
    reverser.discardedClockValue = state.clock
    state.enpassant = None
    state.clock = 0
  }

  override def revertPieceLocations(state: Board, reverser: MoveReverser) {
    val activePawn = Piece(state.active)(0)
    val passivePawn = Piece(state.passive)(0)
    val plocs = state.pieceLocations
    plocs.removeSquare(activePawn, target)
    plocs.addSquare(activePawn, source)
    plocs.addSquare(passivePawn, enpassantSquare)
  }
  
  // Object API
  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[EnpassantMove] && {
      val other = obj.asInstanceOf[EnpassantMove]
      source == other.source && target == other.target
    }
  }
  
  override def hashCode(): Int = (source, target).##
}

object EnpassantMove
{
  def apply(source: Square, target: Square) = {
    new EnpassantMove(source, target)
  }
  
  def apply(source: String, target: String) = {
    new EnpassantMove(Square(source), Square(target))
  }
}

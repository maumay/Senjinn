package senjinn.moves

import senjinn.base.{CastleZone}
import senjinn.base.pieces._
import senjinn.board.{BoardState, MoveReverser}
import senjinn.base.CastleZone.{setOfWhiteZones, setOfBlackZones}

/**
 * Represents the act of castling in a chess game.
 */
final class CastleMove private[moves](val zone: CastleZone) extends ChessMove
{
  // ChessMove API
  override val (source, target) = (zone.kingSrc, zone.kingTarg)
  override val rightsRemoved = if (zone.isWhiteZone) setOfWhiteZones else setOfBlackZones
  override val castleCommand = Some(zone)
  override val pieceDeveloped = None

  override def toCompactString = zone.toString

  override def updatePieceLocations(state: BoardState, reverser: MoveReverser) {
    val king = ChessPiece(state.active)(5)
    val rook = ChessPiece(state.active)(3)
    val plocs = state.pieceLocations
    plocs.removeSquare(king, source)
    plocs.addSquare(king, target)
    plocs.removeSquare(rook, zone.rookSrc)
    plocs.addSquare(rook, zone.rookTarg)
    reverser.pieceTaken = None
    reverser.discardedEnpassant = state.enpassant
    state.enpassant = None
    reverser.discardedClockValue = state.clock
    state.clock += 1
  }

  override def revertPieceLocations(state: BoardState, reverser: MoveReverser) {
    val king = ChessPiece(state.active)(5)
    val rook = ChessPiece(state.active)(3)
    val plocs = state.pieceLocations
    plocs.addSquare(king, source)
    plocs.removeSquare(king, target)
    plocs.addSquare(rook, zone.rookSrc)
    plocs.removeSquare(rook, zone.rookTarg)
  }
  
  // Object API
  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[CastleMove] && zone == obj.asInstanceOf[CastleMove].zone
  }
  
  override def hashCode(): Int = zone.##
}

object CastleMove
{
  def apply(identifier: String) = {
    castleMove(CastleZone(identifier))
  }
}

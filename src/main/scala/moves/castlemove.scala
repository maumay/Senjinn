package senjinn.moves

import senjinn.base.{CastleZone}
import senjinn.pieces._
import senjinn.board.{BoardState, MoveReverser}

class CastleMove(val zone: CastleZone) extends ChessMove
{
  val (source, target) = (zone.kingSrc, zone.kingTarg)

  val rightsRemoved = zone match {
    case x if x.isWhiteZone => CastleZone.setOfAllWhiteZones
    case _ => CastleZone.setOfAllBlackZones
  }

  val pieceDeveloped = None

  def toCompactString = zone.toString

  def updatePieceLocations(state: BoardState, reverser: MoveReverser) {
    val king = ChessPiece(state.active)(5)
    val rook = ChessPiece(state.active)(3)
    val plocs = state.plocs
    plocs.removeSquare(king, source)
    plocs.addSquare(king, target)
    plocs.removeSquare(rook, zone.rookSrc)
    plocs.addSquare(rook, zone.rookTarg)
    state.cstatus.setStatus(zone)
    reverser.pieceTaken = None
    reverser.discardedEnpassant = state.enpassant
    state.enpassant = None
    reverser.discardedClockValue = state.clock
    state.clock += 1
  }

  def revertPieceLocations(state: BoardState, reverser: MoveReverser) {
    val king = ChessPiece(state.active)(5)
    val rook = ChessPiece(state.active)(3)
    val plocs = state.plocs
    plocs.addSquare(king, source)
    plocs.removeSquare(king, target)
    plocs.addSquare(rook, zone.rookSrc)
    plocs.removeSquare(rook, zone.rookTarg)
    state.cstatus.removeStatus(zone)
  }
}

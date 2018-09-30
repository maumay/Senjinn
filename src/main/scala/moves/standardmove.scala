package senjinn.moves

import java.lang.Math.abs

import senjinn.base.{BoardSquare, SquareSet, CastleZone}
import senjinn.base.BoardSquare._
import senjinn.base.CastleZone._
import senjinn.base.DevPiece._
import senjinn.board.{BoardState, MoveReverser}

class StandardMove(val source: BoardSquare, val target: BoardSquare) extends ChessMove
{
  val rightsRemoved = matchRightsRemoved(source) ++ matchRightsRemoved(target)

  private def matchRightsRemoved(sq: BoardSquare) = {
    sq match {
      case x if x == a1 => setOfWqZone
      case x if x == e1 => setOfAllWhiteZones
      case x if x == h1 => setOfWkZone
      case x if x == h8 => setOfBkZone
      case x if x == e8 => setOfAllBlackZones
      case x if x == a8 => setOfBqZone
      case _ => setOfNoZones
    }
  }

  val pieceDeveloped = startSquareMap.get(source)

  def toCompactString = s"S$source$target"

  def updatePieceLocations(state: BoardState, reverser: MoveReverser) {
    val plocs = state.plocs
    val moving = plocs.pieceAt(source, state.active).get
    val removing = plocs.pieceAt(target, state.passive)
    plocs.removeSquare(moving, source)
    plocs.addSquare(moving, target)
    if (removing.isDefined) {plocs.removeSquare(removing.get, target)}
    reverser.pieceTaken = removing

    reverser.discardedEnpassant = state.enpassant
    if (moving.isPawn && abs(target.index - source.index) == 16) {
      val epindex = source.index + (target.index - source.index) / 2
      state.enpassant = Some(BoardSquare(epindex))
    }
    else {
      state.enpassant = None
    }

    reverser.discardedClockValue = state.clock
    if (moving.isPawn || removing.isDefined) {state.clock = 0}
    else {state.clock += 1}
  }

  def revertPieceLocations(state: BoardState, reverser: MoveReverser) {
    val plocs = state.plocs
    val previouslyMoved = plocs.pieceAt(target, state.active).get
    plocs.removeSquare(previouslyMoved, target)
    plocs.addSquare(previouslyMoved, source)
    reverser.pieceTaken.foreach(plocs.addSquare(_, target))
  }
}

object StandardMove
{
}

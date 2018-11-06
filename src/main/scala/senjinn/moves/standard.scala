package senjinn.moves

import java.lang.Math.abs

import senjinn.base.ImplicitAreaConverters.{ boardsquare2long }
import senjinn.base.{Square, SquareSet, CastleZone, DevPiece, Dir}
import senjinn.base.Square._
import senjinn.board.{BoardState, MoveReverser}

/**
 * Represents the act of a 'standard' move in a chess game.
 * I.e. one which is not castling, enpassant or pawn promotion.
 */
class StandardMove private[moves](val source: Square, val target: Square) extends ChessMove
{
  // StandardMove specifics
  val cord: SquareSet = Dir.ofLineConnecting(source, target) match {
    case Some(d) => SquareSet(source.allSquares(d, 8).takeWhile(_ != target).foldLeft(0L)(_ | _))
    case None    => SquareSet()
  }
  
  // ChessMove API
  override val castleCommand = None
  override val pieceDeveloped = DevPiece.startSquareMap.get(source)
  override val rightsRemoved = getRightsRemoved(source) ++ getRightsRemoved(target)

  private def getRightsRemoved(sq: Square) = sq match {
    case x if x == a1 => CastleZone.setOfWqZone
    case x if x == e1 => CastleZone.setOfWhiteZones
    case x if x == h1 => CastleZone.setOfWkZone
    case x if x == h8 => CastleZone.setOfBkZone
    case x if x == e8 => CastleZone.setOfBlackZones
    case x if x == a8 => CastleZone.setOfBqZone
    case _            => CastleZone.setOfNoZones
  }
  
  override def updatePieceLocations(state: BoardState, reverser: MoveReverser) {
    val plocs = state.pieceLocations
    val moving = plocs.pieceAt(source, state.active).get
    val removing = plocs.pieceAt(target, state.passive)
    plocs.removeSquare(moving, source)
    plocs.addSquare(moving, target)
    removing foreach {plocs.removeSquare(_, target)}
    reverser.pieceTaken = removing

    reverser.discardedEnpassant = state.enpassant
    if (moving.isPawn && abs(target.index - source.index) == 16) {
      val epindex = source.index + (target.index - source.index) / 2
      state.enpassant = Some(Square(epindex))
    }
    else {
      state.enpassant = None
    }

    reverser.discardedClockValue = state.clock
    if (moving.isPawn || removing.isDefined) {state.clock = 0}
    else {state.clock += 1}
  }

  override def revertPieceLocations(state: BoardState, reverser: MoveReverser) {
    val plocs = state.pieceLocations
    val previouslyMoved = plocs.pieceAt(target, state.active).get
    plocs.removeSquare(previouslyMoved, target)
    plocs.addSquare(previouslyMoved, source)
    reverser.pieceTaken foreach {plocs.addSquare(_, target)}
  }
  
  override def toCompactString = s"S$source$target"
  
  // Object API
  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[StandardMove] && {
      val other = obj.asInstanceOf[StandardMove]
      (source, target) == (other.source, other.target)
    }
  }
  
  override def hashCode(): Int = (source, target).##
}

object StandardMove
{
  def apply(source: Square, target: Square): StandardMove = standardMove(source, target)
  
  def apply(source: String, target: String): StandardMove = apply(Square(source), Square(target))
}

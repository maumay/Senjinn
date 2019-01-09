package senjinn.moves

import java.lang.Math.abs

import senjinn.base.{boardsquare2long}
import senjinn.base.{Square, SquareSet, CastleZone, DevPiece, Dir}
import senjinn.board.{Board, MoveReverser}

/**
 * Represents the act of a 'standard' move in a chess game.
 * I.e. one which is not castling, enpassant or pawn promotion.
 */
class StandardMove private[moves](val source: Square, val target: Square) extends Move
{
  // StandardMove specifics
  val cord: SquareSet = {
    val inner = Dir.ofLineConnecting(source, target) match {
      case Some(d) => SquareSet(source.allSquares(d, 8).takeWhile(_ != target).foldLeft(0L)(_ | _))
      case None    => SquareSet()
    }
    source | inner | target
  }
  
  // ChessMove API
  override val castleCommand = None
  override val pieceDeveloped = DevPiece(source)
  override val rightsRemoved = {
    val rmBySource = getRightsRemoved(source)
    val rmByTarget = getRightsRemoved(target)
    (rmBySource.size, rmByTarget.size) match {
      case (0, _) => rmByTarget
      case (_, 0) => rmBySource
      case _      => rmBySource ++ rmByTarget
    }
  }

  private def getRightsRemoved(sq: Square) = sq match {
    case Square.a1 => CastleZone.whiteQueenSet
    case Square.e1 => CastleZone.whiteSet
    case Square.h1 => CastleZone.whiteKingSet
    case Square.h8 => CastleZone.blackKingSet
    case Square.e8 => CastleZone.blackSet
    case Square.a8 => CastleZone.blackQueenSet
    case _          => CastleZone.emptySet
  }
  
  override def updatePieceLocations(state: Board, reverser: MoveReverser) {
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

  override def revertPieceLocations(state: Board, reverser: MoveReverser) {
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
  
  override def toString(): String = toCompactString
}

object StandardMove
{
  def apply(source: Square, target: Square): StandardMove = standardMove(source, target)
  
  def apply(source: String, target: String): StandardMove = apply(Square(source), Square(target))
}

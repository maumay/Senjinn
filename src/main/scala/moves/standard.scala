package senjinn.moves

import java.lang.Math.abs

import senjinn.base.{Square, SquareSet, CastleZone, DevPiece, Dir}
import senjinn.base.Square._
import senjinn.board.{BoardState, MoveReverser}

class StandardMove(val source: Square, val target: Square) extends ChessMove
{
  val rightsRemoved = {
    val rightsMatcher = (sq: Square) => sq match {
      case x if x == a1 => CastleZone.setOfWqZone
      case x if x == e1 => CastleZone.setOfWhiteZones
      case x if x == h1 => CastleZone.setOfWkZone
      case x if x == h8 => CastleZone.setOfBkZone
      case x if x == e8 => CastleZone.setOfBlackZones
      case x if x == a8 => CastleZone.setOfBqZone
      case _ => CastleZone.setOfNoZones
    }
    rightsMatcher(source) ++ rightsMatcher(target)
  }

  val castleCommand = None

  val pieceDeveloped = DevPiece.startSquareMap.get(source)

  val cord: SquareSet = {
    val dir = Dir.all.find(source.allSquares(_, 8).contains(target)).get
    SquareSet(source.allSquares(dir, 8).takeWhile(_ != target).foldLeft(0L)(_|_.loc))
  }

  def toCompactString = s"S$source$target"

  def updatePieceLocations(state: BoardState, reverser: MoveReverser) {
    val plocs = state.plocs
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

  def revertPieceLocations(state: BoardState, reverser: MoveReverser) {
    val plocs = state.plocs
    val previouslyMoved = plocs.pieceAt(target, state.active).get
    plocs.removeSquare(previouslyMoved, target)
    plocs.addSquare(previouslyMoved, source)
    reverser.pieceTaken foreach {plocs.addSquare(_, target)}
  }
}

object StandardMove
{
  def apply(source: Square, target: Square) = new StandardMove(source, target)
}

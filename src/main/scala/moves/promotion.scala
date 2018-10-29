package senjinn.moves

import senjinn.base.{Square, CastleZone}
import senjinn.board.{BoardState, MoveReverser}
import senjinn.pieces.{ChessPiece}

class PromotionMove private[moves](val source: Square, val target: Square, val piecetype: Char)
    extends ChessMove
{
  val rightsRemoved = CastleZone.setOfNoZones
  val castleCommand = None
  val pieceDeveloped = None

  def toCompactString = s"$source$target$piecetype"

  def updatePieceLocations(state: BoardState, reverser: MoveReverser) {
    val pt2is = PromotionMove.piecetype2indexshift
    val activePawn = ChessPiece(state.active)(0)
    val promotedPiece = ChessPiece.all(activePawn.index + pt2is(piecetype))
    val plocs = state.plocs
    plocs.removeSquare(activePawn, source)
    plocs.addSquare(promotedPiece, target)
    reverser.pieceTaken = plocs.pieceAt(target, state.passive)
    reverser.pieceTaken foreach {plocs.removeSquare(_, target)}
    reverser.discardedEnpassant = state.enpassant
    reverser.discardedClockValue = state.clock
    state.enpassant = None
    state.clock = 0
  }

  def revertPieceLocations(state: BoardState, reverser: MoveReverser) {
    val pt2is = PromotionMove.piecetype2indexshift
    val activePawn = ChessPiece(state.active)(0)
    val promotedPiece = ChessPiece.all(activePawn.index + pt2is(piecetype))
    val plocs = state.plocs
    plocs.removeSquare(promotedPiece, target)
    plocs.addSquare(activePawn, source)
    reverser.pieceTaken foreach {plocs.addSquare(_, target)}
  }
}

object PromotionMove
{
  def apply(source: Square, target: Square, piecetype: Char) = {
    require(piecetype2indexshift contains piecetype)
    new PromotionMove(source, target, piecetype)
  }
  
  def apply(source: Square, target: Square, piecetype: String) = {
    val piecechar = piecetype.toLowerCase.charAt(0)
    require(piecetype2indexshift contains piecechar)
    new PromotionMove(source, target, piecechar)
  }

  val piecetype2indexshift = Map[Char, Int](
    'n' -> 1,
    'b' -> 2,
    'r' -> 3,
    'q' -> 4)
}

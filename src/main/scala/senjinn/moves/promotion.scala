package senjinn.moves

import senjinn.base.{Square, CastleZone}
import senjinn.board.{BoardState, MoveReverser}
import senjinn.base.pieces.{ChessPiece}

/**
 * Represents the act of pawn promotion in a chess game.
 */
final class PromotionMove private[moves](val source: Square, val target: Square, val piecetype: Char)
    extends ChessMove
{
  // ChessMove API
  override val rightsRemoved = CastleZone.setOfNoZones
  override val castleCommand = None
  override val pieceDeveloped = None

  override def toCompactString = s"$source$target$piecetype"

  override def updatePieceLocations(state: BoardState, reverser: MoveReverser) {
    val pt2is = PromotionMove.piecetype2indexshift
    val activePawn = ChessPiece(state.active)(0)
    val promotedPiece = ChessPiece.all(activePawn.index + pt2is(piecetype))
    val plocs = state.pieceLocations
    plocs.removeSquare(activePawn, source)
    plocs.addSquare(promotedPiece, target)
    reverser.pieceTaken = plocs.pieceAt(target, state.passive)
    reverser.pieceTaken foreach {plocs.removeSquare(_, target)}
    reverser.discardedEnpassant = state.enpassant
    reverser.discardedClockValue = state.clock
    state.enpassant = None
    state.clock = 0
  }

  override def revertPieceLocations(state: BoardState, reverser: MoveReverser) {
    val pt2is = PromotionMove.piecetype2indexshift
    val activePawn = ChessPiece(state.active)(0)
    val promotedPiece = ChessPiece.all(activePawn.index + pt2is(piecetype))
    val plocs = state.pieceLocations
    plocs.removeSquare(promotedPiece, target)
    plocs.addSquare(activePawn, source)
    reverser.pieceTaken foreach {plocs.addSquare(_, target)}
  }
  
  // Object API
  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[PromotionMove] && {
      val other = obj.asInstanceOf[PromotionMove]
      (source, target, piecetype) == (other.source, other.target, other.piecetype)
    }
  }
  
  override def hashCode(): Int = (source, target, piecetype).##
}

object PromotionMove
{
  def apply(source: Square, target: Square, piecetype: Char): PromotionMove = {
    require(piecetype2indexshift contains piecetype)
    new PromotionMove(source, target, piecetype)
  }
  
  def apply(source: Square, target: Square, piecetype: String): PromotionMove = {
    val piecechar = piecetype.toLowerCase.charAt(0)
    require(piecetype2indexshift contains piecechar)
    new PromotionMove(source, target, piecechar)
  }
  
  def apply(source: String, target: String, piecetype: String): PromotionMove = {
    apply(Square(source), Square(target), piecetype)
  }

  val piecetype2indexshift = Map[Char, Int](
    'n' -> 1,
    'b' -> 2,
    'r' -> 3,
    'q' -> 4)
}

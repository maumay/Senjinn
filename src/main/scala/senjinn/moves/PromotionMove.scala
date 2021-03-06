package senjinn.moves

import senjinn.base.{Square, CastleZone, Piece}
import senjinn.board.{Board, MoveReverser}

/**
 * Represents the act of pawn promotion in a chess game.
 */
final class PromotionMove private[moves](val source: Square, val target: Square, val piecetype: Char)
    extends Move
{
  // ChessMove API
  override val rightsRemoved = CastleZone.emptySet
  override val castleCommand = None
  override val pieceDeveloped = None

  override def toCompactString = s"$source$target$piecetype"

  override def updatePieceLocations(state: Board, reverser: MoveReverser) {
    val pt2is = PromotionMove.piecetype2indexshift
    val activePawn = Piece(state.active)(0)
    val promotedPiece = Piece.values(activePawn.index + pt2is(piecetype))
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

  override def revertPieceLocations(state: Board, reverser: MoveReverser) {
    val pt2is = PromotionMove.piecetype2indexshift
    val activePawn = Piece(state.active)(0)
    val promotedPiece = Piece.values(activePawn.index + pt2is(piecetype))
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
  
  override def toString(): String = toCompactString
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
    (Square(source), Square(target)) match {
      case (Some(s), Some(t)) => apply(s, t, piecetype)
      case _                  => throw new RuntimeException
    }
  }
  
  def apply(source: Square, dest: Square): Iterator[PromotionMove] = {
    Iterator(
        new PromotionMove(source, dest, 'n'),
        new PromotionMove(source, dest, 'b'),
        new PromotionMove(source, dest, 'r'),
        new PromotionMove(source, dest, 'q'))
  }

  val piecetype2indexshift = Map[Char, Int](
    'n' -> 1,
    'b' -> 2,
    'r' -> 3,
    'q' -> 4)
}

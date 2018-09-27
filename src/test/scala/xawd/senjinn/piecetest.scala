package xawd.senjinn

import org.scalatest.FlatSpec
import xawd.senjinn.pieces._
import xawd.senjinn.{PieceMovementDirs => pmd}
import xawd.senjinn.ImplicitAreaConverters._


class IndexAlignmentTest extends FlatSpec
{
  "Piece indices" must "be consistent between the different sides" in {
    assert(WhitePawn.index == BlackPawn.index - 6)
    assert(WhiteKnight.index == BlackKnight.index - 6)
    assert(WhiteBishop.index == BlackBishop.index - 6)
    assert(WhiteRook.index == BlackRook.index - 6)
    assert(WhiteQueen.index == BlackQueen.index - 6)
    assert(WhiteKing.index == BlackKing.index - 6)
  }
}

object ConstraintWhitePawn extends Moveable
{
  def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    loc.allSquares(pmd("wpa"), 1).foldLeft(SquareSet())(_|_)
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    val all = whites | blacks
    val push = loc.nextSquare(Dir.n).filterNot(_.intersects(all))
    val secondpush = push.filter(sq => 7 < sq.index && sq.index < 16)
      .flatMap(_.nextSquare(Dir.n))
      .filterNot(_.intersects(all))
    val both = Seq(push, secondpush).filter(_.isDefined).map(_.get.loc).fold(0L)(_|_)
    SquareSet(both) | getAttackset(loc, whites, blacks)
  }
}

object ConstraintWhiteKnight extends Moveable
{
  def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
}

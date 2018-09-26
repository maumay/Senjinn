package xawd.senjinn

import org.scalatest.FlatSpec
import xawd.senjinn.pieces._

class IndexAlignmentTest extends FlatSpec
{
  "Piece indices" must "be consistent between the different sides" in {
    require(WhitePawn.index == BlackPawn.index - 6)
    require(WhiteKnight.index == BlackKnight.index - 6)
    require(WhiteBishop.index == BlackBishop.index - 6)
    require(WhiteRook.index == BlackRook.index - 6)
    require(WhiteQueen.index == BlackQueen.index - 6)
    require(WhiteKing.index == BlackKing.index - 6)
  }
}

object ConstraintWhitePawn extends Moveable
{
  def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {

    

    throw new RuntimeException
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

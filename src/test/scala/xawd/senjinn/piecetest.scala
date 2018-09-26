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
    throw new RuntimeException
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

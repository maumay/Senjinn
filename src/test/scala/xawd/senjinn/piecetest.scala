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

class MoveIntegrationTest extends FlatSpec
{
  val testResource = (getClass.getPackage, "movementIntegrationTestData")

  val positions = processResource(testResource, {buf =>
    import scala.collection.JavaConversions._, java.lang.Long.parseUnsignedLong
    val lines = buf.lines.collect(java.util.stream.Collectors.toList()).toVector
    val hex = raw"(?<=:)[1-9a-f]+[0-9a-f]*".r
    val hexMatched = lines.map(hex.findAllMatchIn(_).map(_.group(0)).toVector)
    hexMatched.map(vec => (parseUnsignedLong(vec(0), 16), parseUnsignedLong(vec(1), 16)))
  })

  def assertGeneratedMovesAreCorrect(loc: BoardSquare, piece: ChessPiece) {
     val consp = Utils.pieceMap(piece)
     positions.foreach(pos => {
       val (w, b) = pos
       assert(piece.getControlset(loc, w, b) == consp.getControlset(loc, w, b))
       assert(piece.getMoveset(loc, w, b) == consp.getMoveset(loc, w, b))
       assert(piece.getAttackset(loc, w, b) == consp.getAttackset(loc, w, b))
     })
  }


  // ChessPiece.all.filterNot(ChessPiece.pawns contains _).foreach(piece => {
  //   BoardSquare.all foreach {sq =>
  //     s"$piece" must s"generate the correct moves at $sq" in {
  //       assertGeneratedMovesAreCorrect(sq, piece)
  //     }
  //   }
  // })

  ChessPiece.rooks.foreach(piece => {
    BoardSquare.all foreach {sq =>
      s"$piece" must s"generate the correct moves at $sq" in {
        assertGeneratedMovesAreCorrect(sq, piece)
      }
    }
  })
}

object Utils
{
  def getSlidingPieceSquaresOfControl(all: SquareSet, loc: BoardSquare, dirs: Iterable[Dir]) = {
    dirs.iterator.flatMap(dir => {
      val squares = loc.allSquares(dir, 8).span(!_.intersects(all))
      squares._1 ++ squares._2.take(1)
    }).foldLeft(SquareSet())(_|_)
  }

  val pieceMap = Map[ChessPiece, Moveable](
    WhitePawn -> ConstraintWhitePawn,
    WhiteKnight -> ConstraintWhiteKnight,
    WhiteBishop -> ConstraintWhiteBishop,
    WhiteRook -> ConstraintWhiteRook,
    WhiteQueen -> ConstraintWhiteQueen,
    WhiteKing -> ConstraintWhiteKing,
    BlackPawn -> ConstraintBlackPawn,
    BlackKnight -> ConstraintBlackKnight,
    BlackBishop -> ConstraintBlackBishop,
    BlackRook -> ConstraintBlackRook,
    BlackQueen -> ConstraintBlackQueen,
    BlackKing -> ConstraintBlackKing)
}

object ConstraintWhitePawn extends Moveable
{
  def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    loc.allSquares(pmd("wpa"), 1).foldLeft(SquareSet())(_|_)
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & blacks
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
    loc.allSquares(pmd("n"), 1).foldLeft(SquareSet())(_|_)
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & blacks
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~whites
  }
}

object ConstraintWhiteBishop extends Moveable
{
  def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    Utils.getSlidingPieceSquaresOfControl(whites | blacks, loc, pmd("b"))
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & blacks
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~whites
  }
}

object ConstraintWhiteRook extends Moveable
{
  def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    Utils.getSlidingPieceSquaresOfControl(whites | blacks, loc, pmd("r"))
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & blacks
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~whites
  }
}

object ConstraintWhiteQueen extends Moveable
{
  def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    Utils.getSlidingPieceSquaresOfControl(whites | blacks, loc, pmd("q"))
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & blacks
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~whites
  }
}

object ConstraintWhiteKing extends Moveable
{
  def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    Utils.getSlidingPieceSquaresOfControl(whites | blacks, loc, pmd("k"))
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & blacks
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~whites
  }
}

object ConstraintBlackPawn extends Moveable
{
  def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    loc.allSquares(pmd("bpa"), 1).foldLeft(SquareSet())(_|_)
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & whites
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    val all = whites | blacks
    val push = loc.nextSquare(Dir.s).filterNot(_.intersects(all))
    val secondpush = push.filter(sq => 47 < sq.index && sq.index < 56)
      .flatMap(_.nextSquare(Dir.s))
      .filterNot(_.intersects(all))
    val both = Seq(push, secondpush).filter(_.isDefined).map(_.get.loc).fold(0L)(_|_)
    SquareSet(both) | getAttackset(loc, whites, blacks)
  }
}

object ConstraintBlackKnight extends Moveable
{
  def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    loc.allSquares(pmd("n"), 1).foldLeft(SquareSet())(_|_)
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & whites
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~blacks
  }
}

object ConstraintBlackBishop extends Moveable
{
  def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    Utils.getSlidingPieceSquaresOfControl(whites | blacks, loc, pmd("b"))
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & whites
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~blacks
  }
}

object ConstraintBlackRook extends Moveable
{
  def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    Utils.getSlidingPieceSquaresOfControl(whites | blacks, loc, pmd("r"))
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & whites
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~blacks
  }
}

object ConstraintBlackQueen extends Moveable
{
  def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    Utils.getSlidingPieceSquaresOfControl(whites | blacks, loc, pmd("q"))
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & whites
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~blacks
  }
}

object ConstraintBlackKing extends Moveable
{
  def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    loc.allSquares(pmd("k"), 1).foldLeft(SquareSet())(_|_)
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & whites
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~blacks
  }
}


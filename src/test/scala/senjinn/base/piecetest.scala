package senjinn.base.pieces

import org.scalatest.FlatSpec

import senjinn.base.{processResource, Square, SquareSet, Dir}
import senjinn.base.{PieceMovementDirs => pmd}
import senjinn.base.pieces.ChessPiece._
import senjinn.base.ImplicitAreaConverters._


class IndexAlignmentTest extends FlatSpec
{
  "Piece indices" must "match the range (0 to 11)" in {
    assert(ChessPiece.values.map(_.index) == (0 to 11).toVector)
  }

  "White piece ordering" must "be pnbrqk" in {
    val expectedWhiteOrder = Vector(WhitePawn, WhiteKnight, WhiteBishop, WhiteRook, WhiteQueen, WhiteKing)
    assert(expectedWhiteOrder == ChessPiece.whites)
    assert(expectedWhiteOrder.map(_.index) == (0 to 5).toVector)
  }

  "Black piece ordering" must "be pnbrqk" in {
    val expectedBlackOrder = Vector(BlackPawn, BlackKnight, BlackBishop, BlackRook, BlackQueen, BlackKing)
    assert(expectedBlackOrder == ChessPiece.blacks)
    assert(expectedBlackOrder.map(_.index) == (6 to 11).toVector)
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

  def assertGeneratedMovesAreCorrect(loc: Square, piece: ChessPiece) {
    val consp = ConstraintPieces.pieceMap(piece)
    positions.foreach(pos => {
      val (w, b) = pos
      assert(piece.getControlset(loc, w, b) == consp.getControlset(loc, w, b))
      assert(piece.getMoveset(loc, w, b) == consp.getMoveset(loc, w, b))
      assert(piece.getAttackset(loc, w, b) == consp.getAttackset(loc, w, b))
    })
  }

  ChessPiece.values.filterNot(ChessPiece.pawns contains _) foreach {piece =>
    Square.values foreach {sq =>
      s"$piece" must s"generate the correct moves at $sq" in {
        assertGeneratedMovesAreCorrect(sq, piece)
      }
    }
  }

  ChessPiece.pawns foreach {pawn =>
    Square.values.drop(8).dropRight(8) foreach {sq =>
      s"$pawn" must s"generate the correct moves at $sq" in {
        assertGeneratedMovesAreCorrect(sq, pawn)
      }
    }
  }
}

object ConstraintPieces
{
  def getSlidingPieceSquaresOfControl(all: SquareSet, loc: Square, dirs: Iterable[Dir]) = {
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
  def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    loc.allSquares(pmd("wpa"), 1).foldLeft(SquareSet())(_|_)
  }
  
  def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & blacks
  }
  
  def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    val all = whites | blacks
    val push = loc.nextSquare(Dir.n).filterNot(_.intersects(all))
    val secondpush = push.filter(_.rank == 2)
      .flatMap(_.nextSquare(Dir.n))
      .filterNot(_.intersects(all))
    val both = Seq(push, secondpush).filter(_.isDefined).map(_.get.loc).fold(0L)(_|_)
    SquareSet(both) | getAttackset(loc, whites, blacks)
  }
}

object ConstraintWhiteKnight extends Moveable
{
  def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    loc.allSquares(pmd("n"), 1).foldLeft(SquareSet())(_|_)
  }
  
  def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & blacks
  }
  
  def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~whites
  }
}

object ConstraintWhiteBishop extends Moveable
{
  def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    ConstraintPieces.getSlidingPieceSquaresOfControl(whites | blacks, loc, pmd("b"))
  }
  
  def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & blacks
  }
  
  def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~whites
  }
}

object ConstraintWhiteRook extends Moveable
{
  def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    ConstraintPieces.getSlidingPieceSquaresOfControl(whites | blacks, loc, pmd("r"))
  }
  
  def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & blacks
  }
  
  def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~whites
  }
}

object ConstraintWhiteQueen extends Moveable
{
  def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    ConstraintPieces.getSlidingPieceSquaresOfControl(whites | blacks, loc, pmd("q"))
  }
  
  def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & blacks
  }
  
  def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~whites
  }
}

object ConstraintWhiteKing extends Moveable
{
  def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    loc.allSquares(pmd("k"), 1).foldLeft(SquareSet())(_|_)
  }
  
  def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & blacks
  }
  
  def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~whites
  }
}

object ConstraintBlackPawn extends Moveable
{
  def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    loc.allSquares(pmd("bpa"), 1).foldLeft(SquareSet())(_|_)
  }
  
  def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & whites
  }
  
  def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    val all = whites | blacks
    val push = loc.nextSquare(Dir.s).filterNot(_.intersects(all))
    val secondpush = push.filter(_.rank == 5)
      .flatMap(_.nextSquare(Dir.s))
      .filterNot(_.intersects(all))
    val both = Seq(push, secondpush).filter(_.isDefined).map(_.get.loc).fold(0L)(_|_)
    SquareSet(both) | getAttackset(loc, whites, blacks)
  }
}

object ConstraintBlackKnight extends Moveable
{
  def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    loc.allSquares(pmd("n"), 1).foldLeft(SquareSet())(_|_)
  }
  
  def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & whites
  }
  
  def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~blacks
  }
}

object ConstraintBlackBishop extends Moveable
{
  def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    ConstraintPieces.getSlidingPieceSquaresOfControl(whites | blacks, loc, pmd("b"))
  }
  
  def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & whites
  }
  
  def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~blacks
  }
}

object ConstraintBlackRook extends Moveable
{
  def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    ConstraintPieces.getSlidingPieceSquaresOfControl(whites | blacks, loc, pmd("r"))
  }
  
  def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & whites
  }
  
  def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~blacks
  }
}

object ConstraintBlackQueen extends Moveable
{
  def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    ConstraintPieces.getSlidingPieceSquaresOfControl(whites | blacks, loc, pmd("q"))
  }
  
  def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & whites
  }
  
  def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~blacks
  }
}

object ConstraintBlackKing extends Moveable
{
  def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    loc.allSquares(pmd("k"), 1).foldLeft(SquareSet())(_|_)
  }
  
  def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & whites
  }
  
  def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~blacks
  }
}


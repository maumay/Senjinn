package xawd.senjinn.eval

import xawd.senjinn.BoardSquare
import xawd.senjinn.ChessPiece


class PieceValues private (private val values: Array[Int]) extends Iterable[Int]
{
  def valueOf(piece: ChessPiece): Int = values(piece.index % 6)
  
  def iterator = values.iterator
}

object PieceValues
{
  val midgame = PieceValues(100, 310, 320, 480, 910, 0)
  val endgame = PieceValues(120, 270, 340, 550, 940, 0)
  
  private def apply(xs: Int*): PieceValues = {
    require(xs.length == 6)
    require(xs.forall(_ > 0))
    new PieceValues(xs.toArray.map(x => (x * 3.5).toInt))
  }
}


class PieceSquareTable private (private val values: Array[Int])
{
  def valueAt(square: BoardSquare): Int = values(square.index)
  
  def invert: PieceSquareTable = {
    new PieceSquareTable((0 until 64).map(i => -values(63 - 8 * (i / 8) - (7 - (i % 8)))).toArray)
  }
}

object PieceSquareTable
{
  import xawd.senjinn.{ loadResource }
  import xawd.senjinn.Side
  
  def pkg = getClass.getPackage
  def midgameLocators = ChessPiece(Side.white).map(p => (pkg, p.shortName + "-midgame"))
  def endgameLocators = ChessPiece(Side.white).map(p => (pkg, p.shortName + "-endgame"))
  
  val midgame = PieceValues.midgame.zip(midgameLocators).map(p => parse(p._1, loadResource(p._2)))
  val endgame = PieceValues.endgame.zip(midgameLocators).map(p => parse(p._1, loadResource(p._2)))
  
  private def parse(piecevalue: Int, lines: Vector[String]): PieceSquareTable = {
    require(lines.length == 8)
    val np = "-?[0-9]+".r
    val parsedLines = lines.reverseMap(np.findAllMatchIn(_).map(_.group(1).toInt).toVector).flatMap(_.reverse)
    PieceSquareTable(piecevalue, parsedLines)
  }
  
  private def apply(pieceValue: Int, locationValues: Iterable[Int]): PieceSquareTable = {
    val res = locationValues.iterator.map(_ + pieceValue).toArray
    require(res.length == 64)
    new PieceSquareTable(res)
  }
}


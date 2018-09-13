package xawd.senjinn.eval

import xawd.senjinn.BoardSquare


class PieceSquareTable private (private val values: Array[Int])
{
  def valueAt(square: BoardSquare): Int = values(square.index)
  
  def invert: PieceSquareTable = {
    new PieceSquareTable((0 until 64).map(i => -values(63 - 8 * (i / 8) - (7 - (i % 8)))).toArray)
  }
}

object PieceSquareTable
{
  private def apply(pieceValue: Int, locationValues: Iterable[Int]): PieceSquareTable = {
    val res = locationValues.iterator.map(_ + pieceValue).toArray
    require(res.length == 64)
    new PieceSquareTable(res)
  }
}


private object TableParser
{
  
}
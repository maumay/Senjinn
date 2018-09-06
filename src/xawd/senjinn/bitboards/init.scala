package xawd.senjinn.bitboards

import xawd.senjinn.SquareSet._
import xawd.senjinn.BoardSquare._
import xawd.senjinn.Direction
import xawd.senjinn.SquareSet
import xawd.senjinn.BoardSquare

object BitboardInit 
{
  val ranks: Vector[SquareSet] = {
    (0 to 7).map(i => (0 to 7).map(j => (1L << 8 * i) << j).reduce(_ | _): SquareSet).toVector
  }
                                 
  val files: Vector[SquareSet] = {
    (0 to 7).map(i => (0 to 7).map(j => (1L << i) << 8 * j).reduce(_ | _): SquareSet).toVector
  }
  
  val diagonals: Vector[SquareSet] = {
    (0 to 14)
    .map(i => if (i < 8) i else 8 *(i - 7) + 7)
    .map(BoardSquare(_))
    .map(sq => sq +: sq.allSquares(Array(Direction.ne)))
    .map(xs => xs.foldLeft(SquareSet())((a, b) => a | b))
    .toVector
  }
  
  
}
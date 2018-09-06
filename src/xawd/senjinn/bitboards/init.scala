package xawd.senjinn.bitboards

import xawd.senjinn.SquareSet._
import xawd.senjinn.BoardSquare._
import xawd.senjinn.Direction
import xawd.senjinn.SquareSet
import xawd.senjinn.BoardSquare

object BitboardInit 
{
  /**
   * The 8 ranks of a chessboard.
   */
  val ranks: Vector[SquareSet] = {
    (0 to 7).map(i => (0 to 7).map(j => (1L << 8 * i) << j).reduce(_ | _): SquareSet).toVector
  }
  
  /**
   * The 8 files of a chessboard.
   */
  val files: Vector[SquareSet] = {
    (0 to 7).map(i => (0 to 7).map(j => (1L << i) << 8 * j).reduce(_ | _): SquareSet).toVector
  }
  
  /**
   * The 15 north-east diagonals on a chessboard, ordered right to left.
   */
  val diagonals: Vector[SquareSet] = {
    (0 to 14)
    .map(i => if (i < 8) i else 8 *(i - 7) + 7)
    .map(BoardSquare(_))
    .map(sq => sq +: sq.allSquares(Array(Direction.ne)))
    .map(xs => xs.foldLeft(SquareSet())((a, b) => a | b))
    .toVector
  }
  
  /**
   * The 15 north-west diagonals on a chessboard, ordered left to right.
   */
  val antidiagonals: Vector[SquareSet] = {
    (0 to 14)
    .map(i => if (i < 8) 7 - i else 8 *(i - 7))
    .map(BoardSquare(_))
    .map(sq => sq +: sq.allSquares(Array(Direction.nw)))
    .map(xs => xs.foldLeft(SquareSet())((a, b) => a | b))
    .toVector
  }
}
package xawd.senjinn.bitboards

import xawd.senjinn.SquareSet._

import xawd.senjinn.BoardSquare._
import xawd.senjinn.Direction
import xawd.senjinn.SquareSet
import xawd.senjinn.BoardSquare
import xawd.senjinn.{foldSquares}

/**
 * Encapsulates the basic areas on a chessboard. More specifically it contains:
 * <ul>
 * <li>The ranks and files</li>
 * <li>The north-east diagonals and north-west diagonals (anti-diagonals)</li>
 * <li>The possible moves on an empty board for each piece type</li>
 * <li>The squares controlled on an empty board for each piece type.</li>
 * </ul>
 */
object BasicBitboard
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
    .map(sq => foldSquares(sq +: sq.allSquares(Array(Direction.ne))))
    .toVector
  }
  
  /**
   * The 15 north-west diagonals on a chessboard, ordered left to right.
   */
  val antidiagonals: Vector[SquareSet] = {
    (0 to 14)
    .map(i => if (i < 8) 7 - i else 8 *(i - 7))
    .map(BoardSquare(_))
    .map(sq => foldSquares(sq +: sq.allSquares(Array(Direction.nw))))
    .toVector
  }
  
  /**
   * The possible moves for each piece types (white and black pawns are 
   * distinguished) and for each square on the board.
   */
  val emptyBoardMoves: Vector[Vector[SquareSet]] = {
    import xawd.senjinn.{ PieceMovementDirs => pmd}
    Vector(
        genWhitePawnMoves,
        genBlackPawnMoves,
        genAllBitboards(pmd("n"), 1),
        genAllBitboards(pmd("b")),
        genAllBitboards(pmd("r")),
        genAllBitboards(pmd("q")),
        genAllBitboards(pmd("k"), 1)
        )
  }
  
  /**
   * The areas of control for each piece types (white and black pawns 
   * are distinguished) and for each square on the board.
   */
  val emptyBoardControl: Vector[Vector[SquareSet]] = {
    import xawd.senjinn.{ PieceMovementDirs => pmd}
    Vector(
        genAllBitboards(pmd("wpa"), 1),
        genAllBitboards(pmd("bpa"), 1),
        emptyBoardMoves(2),
        emptyBoardMoves(3),
        emptyBoardMoves(4),
        emptyBoardMoves(5),
        emptyBoardMoves(6)
        )
  }
  
  private def genWhitePawnMoves: Vector[SquareSet] = {
    import xawd.senjinn.{ PieceMovementDirs => pmd}
    val arr = genAllBitboards(pmd("wpm"), 1).toArray
    (8 to 15).foreach(i => arr(i) |= BoardSquare(i + 16))
    arr.toVector
  }
  
  private def genBlackPawnMoves: Vector[SquareSet] = {
    import xawd.senjinn.{ PieceMovementDirs => pmd}
    val arr = genAllBitboards(pmd("bpm"), 1).toArray
    (48 to 56).foreach(i => arr(i) |= BoardSquare(i - 16))
    arr.toVector
  }
  
  private def genAllBitboards(dirs: Iterable[Direction], proximity: Int = 8): Vector[SquareSet] = {
    BoardSquare.values.map(sq => foldSquares(sq.allSquares(dirs, proximity))).toVector
  }
}
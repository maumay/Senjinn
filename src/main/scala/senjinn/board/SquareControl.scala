package senjinn.board

import senjinn.base.{ SquareSet, Piece, BasicBitboards, Side }

/**
 * Exposes functions for computing sets of squares controlled by
 * pieces on a board.
 */
object SquareControl {

  def compute(board: Board, side: Side): SquareSet = {
    Piece(side).foldLeft(0L)(_ | compute(board, _))
  }

  def compute(board: Board, piece: Piece): SquareSet = if (piece.isPawn) {
    computePawnControl(board, piece)
  } else {
    val plocs = board.pieceLocations
    val enemyKingLoc = plocs.kingLoc(Side.other(piece.side))
    val (whites, blacks) = (plocs.whites - enemyKingLoc, plocs.blacks - enemyKingLoc)
    board.pieceLocations.locs(piece).squares
      .map(piece.getControlset(_, whites, blacks)).foldLeft(0L)(_ | _)
  }

  // We can take a shortcut for pawns
  private val aFileRemover = ~BasicBitboards.file(7)
  private val hFileRemover = ~BasicBitboards.file(0)

  private def computePawnControl(board: Board, pawn: Piece): SquareSet = {
    val pawnlocs = board.pieceLocations.locs(pawn)
    if (pawn.isWhite) {
      ((pawnlocs & aFileRemover) << 9) | ((pawnlocs & hFileRemover) << 7)
    } else {
      ((pawnlocs & aFileRemover) >>> 7) | ((pawnlocs & hFileRemover) >>> 9)
    }
  }
}
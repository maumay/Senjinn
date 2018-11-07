package senjinn.board

import senjinn.moves.{ChessMove, CastleMove}
import senjinn.base.{CastleZone, SquareSet}
import senjinn.base.BasicBitboards

/**
 * 
 */
object LegalMoves {
  
  private def computeMoves(board: Board, forceAttacks: Boolean): Iterator[ChessMove] = {
    val (active, passive) = (board.active, board.passive)
    val plocs = board.pieceLocations
    val activeKingLoc = plocs.kingLoc(active)
    
    val passiveControl = SquareControl.compute(board, passive)
    val pinnedPieces = PinnedPieces.compute(board)
    val inCheck = passiveControl intersects activeKingLoc
    val castlingAllowed = !inCheck && !forceAttacks && board.castleStatus.status(active).isEmpty
    
    var moves = if (castlingAllowed) computeCastlingMoves(board, passiveControl) else Iterator()
    val moveAreaConstraint = if (forceAttacks) plocs.locs(passive) else BasicBitboards.universal
    
    
    throw new RuntimeException
  }
  
  private def computeCastlingMoves(board: Board, passiveControl: SquareSet): Iterator[ChessMove] = {
    val availableRights = CastleZone.completeSet.iterator.filter(_.isWhiteZone == board.active.isWhite)
    val (ap, pc) = (board.pieceLocations.all, passiveControl)
    availableRights
    .filterNot(z => (ap intersects z.requiredClear) || (pc intersects z.requiredUncontrolled))
    .map(CastleMove(_))
  }
}
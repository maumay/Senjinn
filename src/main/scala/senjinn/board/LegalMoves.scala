package senjinn.board

import senjinn.moves.{Move, CastleMove}
import senjinn.base.{CastleZone, SquareSet, Square, Piece}
import senjinn.base.BasicBitboards
import senjinn.moves.PromotionMove
import senjinn.moves.StandardMove

/**
 * 
 */
object LegalMoves {
  
  private def computeMoves(board: Board, forceAttacks: Boolean): Iterator[Move] = {
    val (active, passive) = (board.active, board.passive)
    val plocs = board.pieceLocations
    val activeKingLoc = plocs.kingLoc(active)
    
    val passiveControl = SquareControl.compute(board, passive)
    val pinnedPieces = PinnedPieces.compute(board)
    val inCheck = passiveControl intersects activeKingLoc
    val castlingAllowed = !inCheck && !forceAttacks && board.castleStatus.status(active).isEmpty
    
    var moves = if (castlingAllowed) computeCastlingMoves(board, passiveControl) else Iterator()
    var moveAreaConstraint = if (forceAttacks) plocs.locs(passive) else BasicBitboards.universal
    
    if (inCheck) {
      val (w, b) = (plocs.whites, plocs.blacks)
      val attackers = Piece(passive).flatMap(p => plocs.locs(p).squares
          .filter(p.getControlset(_, w, b) intersects activeKingLoc).map((p, _)))
      if (attackers.size > 1) {
        moveAreaConstraint = SquareSet()
      }
      else {
        val attacker = attackers.head
        moveAreaConstraint &= computeBlockingSquares(activeKingLoc, attacker)
      }
    }
    
    
    
    throw new RuntimeException
  }
  
  private def computeNonKingMoves(board: Board, piece: Piece, 
      pinnedPieces: Map[Square, SquareSet], areaConstraint: SquareSet): Iterator[Move] = {
    if (areaConstraint.size == 0) {
      Iterator()
    }
    else {
      val plocs = board.pieceLocations
      val (w, b) = (plocs.whites, plocs.blacks)
      throw new RuntimeException
    }
  }
  
  private def computeBlockingSquares(kingLoc: Square, attacker: (Piece, Square)): SquareSet = {
    if (attacker._1.isSlider) StandardMove(kingLoc, attacker._2).cord else attacker._2
  }
  
  private def computeKingMoves(board: Board, location: Square, areaConstraint: SquareSet): Iterator[Move] = {
    val plocs = board.pieceLocations
    val (whites, blacks) = (plocs.whites, plocs.blacks)
    val king = Piece(board.active).last
    bitboard2moves(king, location, king.getMoveset(location, whites, blacks) & areaConstraint)
  }
  
  private def computeCastlingMoves(board: Board, passiveControl: SquareSet): Iterator[Move] = {
    val availableRights = CastleZone.completeSet.iterator.filter(_.isWhiteZone == board.active.isWhite)
    val (ap, pc) = (board.pieceLocations.all, passiveControl)
    availableRights
    .filterNot(z => (ap intersects z.requiredClear) || (pc intersects z.requiredUncontrolled))
    .map(CastleMove(_))
  }
  
  private def bitboard2moves(piece: Piece, source: Square, moves: SquareSet): Iterator[Move] = {
    if (piece.isPawn && piece.side.penultimatePawnRank == source.rank) {
      moves.squares.flatMap(dest => PromotionMove(source, dest))
    }
    else {
      moves.squares.map(dest => StandardMove(source, dest))
    }
  }
}
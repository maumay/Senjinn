package senjinn.board

import senjinn.moves.{Move, CastleMove}
import senjinn.base.{CastleZone, SquareSet, Square, Piece, Dir}
import senjinn.base.BasicBitboards
import senjinn.moves.PromotionMove
import senjinn.moves.StandardMove
import senjinn.moves.EnpassantMove

/**
 * 
 */
object LegalMoves {
  
  // API
  def computeMoves(board: Board): Iterator[Move] = computeMoves(board, false)
  def computeAttacks(board: Board): Iterator[Move] = computeMoves(board, true)
  
  // Implementation
  private def computeMoves(board: Board, forceAttacks: Boolean): Iterator[Move] = {
    val (active, passive) = (board.active, board.passive)
    val plocs = board.pieceLocations
    val activeKingLoc = plocs.kingLoc(active)
    
    val passiveControl = SquareControl.compute(board, passive)
    val pinnedPieces = PinnedPieces.compute(board)
    val inCheck = passiveControl intersects activeKingLoc
    val castlingAllowed = !inCheck && !forceAttacks && board.castleStatus.status(active).isEmpty
    
    val castleMoves = if (castlingAllowed) computeCastlingMoves(board, passiveControl) else Iterator()
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
    
    val nonKingMoves = Piece(active).iterator.take(5)
    .flatMap(computeNonKingMoves(board, _, pinnedPieces, moveAreaConstraint))
    val kingConstraint = if (forceAttacks) plocs.locs(passive) \ passiveControl else ~passiveControl
    castleMoves ++ nonKingMoves ++ computeKingMoves(board, activeKingLoc, kingConstraint)
  }
  
  private def computeNonKingMoves(board: Board, piece: Piece, 
      pinnedPieces: Map[Square, SquareSet], areaConstraint: SquareSet): Iterator[Move] = {
    if (areaConstraint.size == 0) {
      Iterator()
    }
    else {
      val plocs = board.pieceLocations
      val (w, b) = (plocs.whites, plocs.blacks)
      val pinnedPartition = plocs.locs(piece).squares.span(pinnedPieces contains _)
      
      val pinnedContribution = pinnedPartition._1.flatMap(sq => {
        val ac = areaConstraint & pinnedPieces(sq)
        bitboard2moves(piece, sq, piece.getMoveset(sq, w, b) & ac)
      })
      
      val notPinnedContribution = pinnedPartition._2.flatMap(sq => {
        bitboard2moves(piece, sq, piece.getMoveset(sq, w, b) & areaConstraint)
      })

      val epContributions = board.enpassant match {
        case None     => Iterator()
        case Some(ep) => {
          val locs = plocs.locs(piece)
          val searchDirs = if (piece.isWhite) List(Dir.sw, Dir.se) else List(Dir.nw, Dir.ne)
          searchDirs.iterator.flatMap(ep.nextSquare(_)).filter(sq => {
            if (locs intersects sq) {
              if (pinnedPieces contains sq) pinnedPieces(sq) intersects ep else true
            } else {
              false
            }
          }).map(sq => EnpassantMove(sq, ep))
        }
      }
      
      pinnedContribution ++ notPinnedContribution ++ epContributions
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
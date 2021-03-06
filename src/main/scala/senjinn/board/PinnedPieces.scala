package senjinn.board

import senjinn.base.{Square, SquareSet}
import senjinn.base.Piece._
import senjinn.moves.StandardMove

object PinnedPieces {
  private val whitePinners = Vector(WhiteBishop, WhiteRook, WhiteQueen)
  private val blackPinners = Vector(BlackBishop, BlackRook, BlackQueen)

  /** Locations of pinned pieces mapped to the areas they are constrained to. */
  type Descriptor = Map[Square, SquareSet]

  def compute(board: Board): Descriptor = {
    val plocs = board.pieceLocations
    val activeLocations = plocs.locs(board.active)
    val allLocations = plocs.all
    val activeKingLocation = plocs.kingLoc(board.active)
    val passivePinners = if (board.active.isWhite) blackPinners else whitePinners

    passivePinners.iterator
      .flatMap(p => plocs.locs(p).squares.filter(p.emptyBoardMoveset(_) intersects activeKingLocation))
      .map(sq => StandardMove(sq, activeKingLocation).cord ^ activeKingLocation ^ sq)
      .filter(cord => (cord & activeLocations).size == 1)//(cord & activeLocations).size == 1 && 
      .map(cord => ((cord & activeLocations).squares.next(), cord))
      .toMap
  }
}


package senjinn

import senjinn.base.{BoardSquare, SquareSet, CastleZone}

package object moves
{
  // Move API
  /**
    * Returns the standard move connecting the source and the
    * target squares. Throws an error if a move which is 
    * impossible is requested.
    */
  def standardMove(src: BoardSquare, target: BoardSquare) = {
    standardCache(src)(target)
  }

  def castleMove(zone: CastleZone) = {
    castleCache(zone)
  }

  def enpassantMove(src: BoardSquare, target: BoardSquare) = {
    new EnpassantMove(src, target)
  }

  /**
    * Returns an iterator over the four possible promotions
    * from the source to the target. Note that no checks on
    * the input legality are made here.
    */
  def promotionMove(src: BoardSquare, target: BoardSquare) = {
    Iterator('q', 'r', 'b', 'n').map(new PromotionMove(src, target, _))
  }

  // Standard move cache implementation
  private type Sq2Standard = Map[BoardSquare, StandardMove]
  private val standardCache: Map[BoardSquare, Sq2Standard] = {
    import senjinn.pieces.{WhiteKnight => wk, WhiteQueen => wq}
    val f = (src: BoardSquare, sqs: SquareSet) => {
      sqs.squares.map(t => (t, StandardMove(src, t))).toMap
    }
    val allsq = BoardSquare.all
    val knightMoves = allsq.map(src => (src, f(src, wk.getEmptyBoardMoveset(src)))).toMap
    val queenMoves  = allsq.map(src => (src, f(src, wq.getEmptyBoardMoveset(src)))).toMap
    knightMoves ++ queenMoves
  }

  private val castleCache: Map[CastleZone, CastleMove] = {
    CastleZone.all.map(z => (z, new CastleMove(z))).toMap
  }
}

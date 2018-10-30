package senjinn

import senjinn.base.{Square, SquareSet, CastleZone}

package object moves
{
  // Move API
  /**
    * Returns the standard move connecting the source and the
    * target squares. Throws an error if a move which is 
    * impossible is requested.
    * 
    * TODO - In some tests I use impossible moves so need
    * to think about how to handle this...
    */
  def standardMove(src: Square, target: Square) = {
    standardCache(src)(target)
  }

  def castleMove(zone: CastleZone) = {
    castleCache(zone)
  }

  def enpassantMove(src: Square, target: Square) = {
    new EnpassantMove(src, target)
  }

  /**
    * Returns an iterator over the four possible promotions
    * from the source to the target. Note that no checks on
    * the input legality are made here.
    */
  def promotionMove(src: Square, target: Square) = {
    Iterator('q', 'r', 'b', 'n').map(new PromotionMove(src, target, _))
  }
  
   // Standard move cache implementation
  private type Sq2Standard = Map[Square, StandardMove]
  private val standardCache: Map[Square, Sq2Standard] = {
    import senjinn.pieces.{WhiteKnight => wk, WhiteQueen => wq}
    val f = (src: Square, sqs: SquareSet) => {
      sqs.squares.map(t => (t, new StandardMove(src, t))).toMap
    }
    val allsq = Square.all
    val knightMoves = allsq.map(src => (src, f(src, wk.getEmptyBoardMoveset(src)))).toMap
    val queenMoves  = allsq.map(src => (src, f(src, wq.getEmptyBoardMoveset(src)))).toMap
    knightMoves ++ queenMoves
  }

  private val castleCache: Map[CastleZone, CastleMove] = {
    CastleZone.all.map(z => (z, new CastleMove(z))).toMap
  }
}

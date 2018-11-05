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
    standardCache2(src.index)(target.index)
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
  
  private val standardCache2: Array[Array[StandardMove]] = {
    import senjinn.base.pieces.{WhiteKnight => n, WhiteQueen => q}
    val sm = new StandardMove(_, _)
    Square.all.map(sq => {
      val array = new Array[StandardMove](64)
      q.getEmptyBoardMoveset(sq).squares.foreach(targ => array(targ.index) = sm(sq, targ))
      n.getEmptyBoardMoveset(sq).squares.foreach(targ => array(targ.index) = sm(sq, targ))
      array
    }).toArray
  }

  private val castleCache: Map[CastleZone, CastleMove] = {
    CastleZone.all.map(z => (z, new CastleMove(z))).toMap
  }
}

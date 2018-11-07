package senjinn.base


/**
 * Supertype of all chesspieces.  
 */
trait Moveable {
  
  /** 
   *  Get the set of squares this piece is controlling given the locations of all pieces
   *  on the board.
   */
  def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet): SquareSet
  
  /** 
   *  Get the set of squares this piece can 'legally' move to which would result in a capture 
   *  of an enemy given the location of all pieces on the board. Note that this method is
   *  <b>not</b> expected to take into account illegal moves where the king is put into check.
   */
  def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet): SquareSet
  
  /** 
   *  Get the set of squares this piece can 'legally' move to given the location of all pieces.
   *  Note that this method is <b>not</b> expected to take into account illegal moves where 
   *  the king is put into check.
   */
  def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet): SquareSet
}
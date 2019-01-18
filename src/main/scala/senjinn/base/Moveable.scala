package senjinn.base


/**
 * Behaviour required for moving on a chess board. 
 */
trait Moveable {
  
  /** 
   *  Compute the set of squares this piece is controlling on the board.
   *  
   *  @param loc the location of this piece
   *  @param whites the locations of all the white pieces.
   *  @param blacks the locations of all the black pieces.
   */
  def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet): SquareSet
  
  /** 
   *  Compute the set of squares this piece can move to which would result
   *  in a capture of an enemy. This method is __not__ expected to take 
   *  into account illegal moves where the king is put into check.
   *  
   *  @param loc the location of this piece
   *  @param whites the locations of all the white pieces.
   *  @param blacks the locations of all the black pieces.
   */
  def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet): SquareSet
  
  /** 
   *  Get the set of squares this piece can move to. This method is __not__ 
   *  expected to take into account illegal moves where the king is put 
   *  into check.
   *  
   *  @param loc the location of this piece
   *  @param whites the locations of all the white pieces.
   *  @param blacks the locations of all the black pieces.
   */
  def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet): SquareSet
}
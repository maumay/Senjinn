package senjinn.base

/**
 * Wrapper for a primitive 64 bit integer which represents a collection of squares
 * on a chess board.
 */
class SquareSet private(val src: Long) extends AnyVal
{
  def |(other: SquareSet) = SquareSet(src | other.src)
  
  def &(other: SquareSet) = SquareSet(src & other.src)
  
  def ^(other: SquareSet) = SquareSet(src ^ other.src)
  
  def -(other: SquareSet) = SquareSet(src & ~other.src)
  
  def <<(shift: Int) = SquareSet(src << shift)
  
  def >>(shift: Int) = SquareSet(src >> shift)
  
  def unary_~ = SquareSet(~src)
  
  def intersects(square: SquareSet): Boolean = {
    (square.src & src) != 0
  }
  
  def size = java.lang.Long.bitCount(src)
  
  def squares: Iterator[Square] = (0 to 63).iterator
                                       .filter(i => ((1L << i) & src) != 0)
                                       .map(Square(_))

  override def toString = src.toString
}

object SquareSet
{
  def apply() = new SquareSet(0L)
  
  def apply(arg: Long) = new SquareSet(arg)
  
  def apply(args: Square*) = new SquareSet(args.foldLeft(0L)(_ | _.loc))
  
}

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
  
  def <<(shift: Int) = SquareSet(src << shift)
  
  def >>(shift: Int) = SquareSet(src >> shift)
  
  def unary_~ = SquareSet(~src)
  
  def intersects(square: Square2): Boolean = {
    (square.loc & src) != 0
  }
  
  def squares: Iterator[Square2] = (0 to 63).iterator
                                       .filter(i => ((1L << i) & src) != 0)
                                       .map(Square2(_))

  override def toString = src.toString
}

object SquareSet
{
  def apply() = new SquareSet(0L)
  
  def apply(arg: Long) = new SquareSet(arg)
  
  def apply(args: Square2*) = new SquareSet(args.foldLeft(0L)(_ | _.loc))
  
}


object ImplicitAreaConverters
{
  implicit def boardsquare2squareset(square: Square2): SquareSet = SquareSet(square.loc)
  
  implicit def long2squareset(x: Long): SquareSet = SquareSet(x)
  
  implicit def squareset2long(s: SquareSet): Long = s.src

  implicit def boardsquare2long(square: Square2): Long = square.loc
}


package xawd.senjinn

class SquareSet private(val squares: Long) extends AnyVal 
{
  def |(other: SquareSet) = new SquareSet(squares | other.squares)
  
  def &(other: SquareSet) = new SquareSet(squares & other.squares)
  
  def ^(other: SquareSet) = new SquareSet(squares ^ other.squares)
  
  def <<(shift: Int) = new SquareSet(squares << shift)
  
  def >>(shift: Int) = new SquareSet(squares >> shift)
  
  def unary_~ = new SquareSet(~squares)
}

object SquareSet
{
  def apply(arg: Long) = new SquareSet(arg)
  
  def apply(args: Long*) = new SquareSet(args.foldLeft(0L)(_ | _))
  
  implicit def boardsquare2squareset(square: BoardSquare): SquareSet = new SquareSet(square.loc)
}

object Test extends App
{
  println(SquareSet(1L))
  println(SquareSet(1L, 2L))
}
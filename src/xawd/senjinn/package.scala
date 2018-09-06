package xawd

package object senjinn 
{
//  type Sq = BoardSquare
//  val  Sq = BoardSquare
  
  val MaxNegatableInt: Int = Integer.MAX_VALUE - 1
  val GameWinValue = MaxNegatableInt / 2
  
  
  val PieceMovementDirs = {
    import xawd.senjinn.Direction._
    val b = Vector(Direction.ne, se, sw, nw)
    val r = Vector(n, e, s, w)
    val q = b ++ r
    
    Map("wpm" -> Vector(n), 
        "wpa" -> Vector(sw, nw),
        "bpm" -> Vector(s),
        "bpa" -> Vector(se, Direction.ne),
        "n"   -> Vector(nne, nee, see, sse, ssw, sww, nww, nnw),
        "b"   -> b,
        "r"   -> r,
        "q"   -> q,
        "k"   -> q
        )
  }
  
  def foldSquares(squares: Iterable[BoardSquare]): SquareSet = {
    foldSquares(squares.iterator)
  }
  
  def foldSquares(squares: Iterator[BoardSquare]): SquareSet = {
    squares.foldLeft(SquareSet())((a, b) => a | b)
  }
  
//  val InitialAlpha = -2 * (MaxNegatableInt / 3)
//  val InitialBeta 
}
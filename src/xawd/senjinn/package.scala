package xawd

package object senjinn 
{
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
  
  def loadResource(cls: java.lang.Class[_], name: String): Vector[String] = {
    import java.util.stream._, scala.collection.JavaConversions._
    processResource(cls, name, buf => buf.lines().collect(Collectors.toList()).toVector)
  }
  
  def processResource[R](cls: java.lang.Class[_], name: String, action: java.io.BufferedReader => R): R = {
    import java.io._
    val absname = "/" + cls.getPackage.getName.replace('.', '/') + "/" + name
    val in = cls.getResourceAsStream(absname)
    autoClose(new BufferedReader(new InputStreamReader(in)))(action)
  }
  
  def autoClose[C <: java.io.Closeable, R](src: C)(action: C => R): R = {
    try {
      action(src)
    }
    finally {
      try {
        src.close()
      }
      catch {
        case _: java.io.IOException => throw new RuntimeException
      }
    }
  }
}
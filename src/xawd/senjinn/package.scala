package xawd

import scala.util.Try
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
  
  
  def loadResource(cls: java.lang.Class[_], name: String): Vector[String] = {
    import java._, collection.JavaConversions._
    val absname = "/" + cls.getPackage.getName.replace('.', '/') + "/" + name
    val in = cls.getResourceAsStream(absname)
    autoClose(new io.BufferedReader(new io.InputStreamReader(in))) { buf => 
      buf.lines().collect(util.stream.Collectors.toList()).toVector
    }
  }
  
//  def loadResourceReader
}
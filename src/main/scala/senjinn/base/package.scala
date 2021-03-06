package senjinn

import senjinn.base.Dir._


package object base
{
  val MaxNegatableInt: Int = Integer.MAX_VALUE - 1
  val GameWinValue = MaxNegatableInt / 2
  
  /**
   * A map describing the directions each piece can legally move/attack.
   */
  val PieceMovementDirs = {
    val b = Vector(Dir.ne, se, sw, nw)
    val r = Vector(n, e, s, w)
    val q = b ++ r
    
    Map("wpm" -> Vector(n), 
        "wpa" -> Vector(Dir.ne, nw),
        "bpm" -> Vector(s),
        "bpa" -> Vector(se, sw),
        "n"   -> Vector(nne, nee, see, sse, ssw, sww, nww, nnw),
        "b"   -> b,
        "r"   -> r,
        "q"   -> q,
        "k"   -> q)
  }

  /**
   * For the given input set this function returns the powerset with each
   * member set folded together using the bitwise or operator.
   */
  def foldedPowerset(input: Vector[Long]): Vector[Long] = input match {
    case head +: tail => {
      val recursed = foldedPowerset(tail)
      recursed ++ recursed.map(sqs => sqs | head)
    }
    case x => Vector(0L)
  }
  
  /*
   * Implicit board area converters
   */
  implicit def boardsquare2squareset(square: Square): SquareSet = SquareSet(square.loc)
  
  implicit def long2squareset(x: Long): SquareSet = SquareSet(x)
  
  implicit def squareset2long(s: SquareSet): Long = s.src

  implicit def boardsquare2long(square: Square): Long = square.loc
  
  /*
   * Resource loading functionality
   */
  type ResourceLocator = (java.lang.Package, String)
  
  def loadResource(locator: ResourceLocator): Vector[String] = {
    import java.util.stream._, scala.collection.JavaConversions._
    processResource(locator, { buf => 
      val xs = buf.lines().collect(Collectors.toList()).toVector
      xs.map(_.trim).filter(line => !line.startsWith("//") && !line.isEmpty())
    })
  }
  
  def processResource[R](locator: ResourceLocator, action: java.io.BufferedReader => R): R = {
    import java.io._
    val absname = "/" + locator._1.getName.replace('.', '/') + "/" + locator._2
    val in = getClass.getResourceAsStream(absname)
    try {
      val buf = new BufferedReader(new InputStreamReader(in))
      autoClose(buf)(action)
    }
    catch {
      case ex: java.lang.NullPointerException => {
        println(locator)
        throw ex
      }
    }
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

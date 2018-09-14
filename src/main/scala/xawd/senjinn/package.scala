package xawd

import xawd.senjinn.ImplicitAreaConverters._


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
  
  /*
   * Resource loading functionality
   */
  type ResourceLocator = (java.lang.Package, String)
  
  def loadResource(locator: ResourceLocator): Vector[String] = {
    import java.util.stream._, scala.collection.JavaConversions._
    processResource(locator, {buf => 
      val xs = buf.lines().collect(Collectors.toList()).toVector
      xs.map(_.trim).filter(!_.startsWith("//"))
    })
  }
  
  def processResource[R](locator: ResourceLocator, action: java.io.BufferedReader => R): R = {
    import java.io._
    val absname = "/" + locator._1.getName.replace('.', '/') + "/" + locator._2
    val in = getClass.getResourceAsStream(absname)
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
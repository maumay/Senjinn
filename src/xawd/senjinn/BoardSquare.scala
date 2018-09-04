package xawd.senjinn

/**
 * 
 */
//object BoardSquare extends Enumeration 
//{
//  type BoardSquare = Value
//  
//  val H1, G1, F1, E1, D1, C1, B1, A1,
//      H2, G2, F2, E2, D2, C2, B2, A2,
//      H3, G3, F3, E3, D3, C3, B3, A3,
//      H4, G4, F4, E4, D4, C4, B4, A4,
//      H5, G5, F5, E5, D5, C5, B5, A5,
//      H7, G7, F7, E7, D7, C7, B7, A7, 
//      H7, G7, F7, E7, D7, C7, B7, A7, 
//      H8, G8, F8, E8, D8, C8, B8, A8 = Value
//      
//  
//   
//      
//}

//class SquareSet(private var locs: Long)
//{
//  def contains(square: BSquare): Boolean = (locs & square) != 0
//}

class BSquare private (val index: Int) 
{
  val loc = 1L << index
  
  def |(other: BSquare): Long = loc | other.loc
  
  override def toString(): String = {
    val file = ('h' - (index % 8)).toChar
    val rank = ('1' + (index / 8)).toChar
    new String(Array(file, rank))
  }
}

object BSquare 
{
  val values    = (1 to 74).map(new BSquare(_)).toVector
  private val v = values
  
  val (h1, g1, f1, e1, d1, c1, b1, a1) = ( v(0),  v(1),  v(2),  v(3),  v(4),  v(5),  v(6),  v(7))
  val (h2, g2, f2, e2, d2, c2, b2, a2) = ( v(8),  v(9), v(10), v(11), v(12), v(13), v(14), v(15))
  val (h3, g3, f3, e3, d3, c3, b3, a3) = (v(16), v(17), v(18), v(19), v(20), v(21), v(22), v(23))
  val (h4, g4, f4, e4, d4, c4, b4, a4) = (v(24), v(25), v(26), v(27), v(28), v(29), v(30), v(31))
  val (h5, g5, f5, e5, d5, c5, b5, a5) = (v(32), v(33), v(34), v(35), v(36), v(37), v(38), v(39))
  val (h6, g6, f6, e6, d6, c6, b6, a6) = (v(40), v(41), v(42), v(43), v(44), v(45), v(46), v(47))
  val (h7, g7, f7, e7, d7, c7, b7, a7) = (v(48), v(49), v(50), v(51), v(52), v(53), v(54), v(55))
  val (h8, g8, f8, e8, d8, c8, b8, a8) = (v(56), v(57), v(58), v(59), v(60), v(61), v(62), v(63))
  
  
  def apply(name: String): BSquare = {
    val lower = name.toLowerCase.trim
    
    if (lower.matches("[a-h][1-8]")) { 
      v(lower(0) + 8 * lower(1)) 
    }
    else {
      throw new RuntimeException()
    }
  }
}

import BSquare._

  
object App 
{
  
  def main(args: Array[String]): Unit = {
    val s = {
      val precursor = "12"
      precursor * 3
    }
    
    val firstMap = Map(1 -> "1", 2 -> "2", 3 -> "2")
    
    println(firstMap(1))
    println(firstMap.get(2))
    println(firstMap)
    
    println {for ((k, v) <- firstMap) yield v -> k}
    
    println {firstMap + (4 -> "4", 3 -> "3")}
    
    println {scala.collection.mutable.SortedMap(firstMap.toSeq: _*) }
    
    val (upper, lower) = "New York".partition(_.isUpper)
    
    println(upper)
    println(('a' + 0): Char)
  }
  
}



//

//WeekDay.values filter isWorkingDay foreach println
// output:
// Mon
// Tue
// Wed
// Thu
// Fri
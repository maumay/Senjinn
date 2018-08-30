package xawd.senjinn

/**
 * 
 */
object BoardSquare extends Enumeration 
{
  type BoardSquare = Value
  
  val H1, G1, F1, E1, D1, C1, B1, A1,
      H2, G2, F2, E2, D2, C2, B2, A2,
      H3, G3, F3, E3, D3, C3, B3, A3,
      H4, G4, F4, E4, D4, C4, B4, A4,
      H5, G5, F5, E5, D5, C5, B5, A5,
      H6, G6, F6, E6, D6, C6, B6, A6, 
      H7, G7, F7, E7, D7, C7, B7, A7, 
      H8, G8, F8, E8, D8, C8, B8, A8 = Value
      
  
}

import BoardSquare._
  
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
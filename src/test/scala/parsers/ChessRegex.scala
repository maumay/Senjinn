package parsers
import scala.util.matching.Regex

/**
 */
object ChessRegex //extends App
{
  private val arrow: String = "\\-\\>"
  
  val singleSquare: Regex = "([a-hA-H][1-8])".r
  val doubleSquare: Regex = s"($singleSquare +$singleSquare)".r
  val cord: Regex = s"($singleSquare$arrow$singleSquare)".r
  val multiTarget: Regex = s"$singleSquare$arrow\\{( *$singleSquare *)\\}".r
  val shorthandMove: Regex = raw"([sScCeEpP]\[[a-hA-H1-8kKqQwW \-\>\{\}]+( [NBRG])?\])".r
  val castleZone = "(([wW][kK])|([wW][qQ])|([bB][kK])|([bB][qQ]))".r
  
  
//  "e4->h6" match {
//    case cord(_*) => println("matched")
//    case _ => println("failed")
//  }
}
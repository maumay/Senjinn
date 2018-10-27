package parsers

import senjinn.base.{Square}
import senjinn.moves.{ChessMove}
import senjinn.parsers.{ChessRegex}

/**
 * 
 */
trait MoveParsing
{
  def parseStandardMoves(encodedMove: String): Vector[ChessMove] = {
    
    throw new RuntimeException
  }
  
  def parseMultiMove(encodedMoves: String): (Square, Vector[Square]) = {
    val em = encodedMoves.trim
    val (cordrx, multitargetrx) = (ChessRegex.cord, ChessRegex.multiTarget)
    
    em match {
      case cordrx(_*) => {
        throw new RuntimeException
      }
      case multitargetrx(_*) => parseMultiMove(em)
      case _ => throw new RuntimeException
    }
  }
  
  private def parseCord(x: String): (Square, Vector[Square]) = {
    val matchedSquares = ChessRegex.singleSquare
    .findAllMatchIn(x)
    .map {m => Square(m.matched)}
    .toVector
    val (start, end) = (matchedSquares.head, matchedSquares.last)
    throw new RuntimeException
  }
  
  private def parseMultiTarget(x: String): (Square, Vector[Square]) = {
    val matchedSquares = ChessRegex.singleSquare
    .findAllMatchIn(x)
    .map {m => Square(m.matched)}
    .toVector
    (matchedSquares.head, matchedSquares.drop(1))
  }
  
  
}
package senjinn.parsers

import senjinn.base.{Square}
import senjinn.moves.{ChessMove, StandardMove}

/**
 * 
 */
trait MoveParsing
{
  private[parsers] def parsePromotionMoves(encodedMove: String): Vector[ChessMove] = {
    throw new RuntimeException
  }
  
  private[parsers] def parseStandardMoves(encodedMove: String): Vector[ChessMove] = {
    if (encodedMove.matches(s"S\\[$ChessRegex.doubleSquare\\]")) {
      val squares = ChessRegex.singleSquare.findAllMatchIn(encodedMove)
      .map(m => Square(m.matched)).toVector
      Vector(StandardMove(squares.head, squares.last))
    }
    else {
      val mmove = parseMultiMove(encodedMove)
      mmove._2.map(target => StandardMove(mmove._1, target))
    }
  }
  
  private[parsers] def parseMultiMove(encodedMoves: String): (Square, Vector[Square]) = {
    val em = encodedMoves.trim
    val (cordrx, multitargetrx) = (ChessRegex.cord, ChessRegex.multiTarget)
    em match {
      case cordrx(_*)        => parseCord(em)
      case multitargetrx(_*) => parseMultiMove(em)
      case _                 => throw new RuntimeException
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
package senjinn.parsers

import senjinn.base.{Square}
import senjinn.moves.{ChessMove, StandardMove, PromotionMove, EnpassantMove}

/**
 * 
 */
trait MoveParsing
{
  private[parsers] def parseEnpassantMove(encodedMove: String): ChessMove = {
    val squares = ChessRegex.singleSquare.findAllIn(encodedMove).map(Square(_)).toVector
    EnpassantMove(squares.head, squares.last)
  }
  
  private[parsers] def parsePromotionMoves(encodedMoves: String): Vector[ChessMove] = {
    val em = encodedMoves
    val promotionresult = "[NBRQ]".r.findAllIn(em).toVector.last
    
    if (em.matches(s"P\\[($ChessRegex.multiTarget|$ChessRegex.cord)$promotionresult\\]")) {
      val (src, targets) = parseMultiMove(em.substring(2, em.length - 3))
      targets.map(target => PromotionMove(src, target, promotionresult))
    }
    else {
      val squares = ChessRegex.singleSquare.findAllIn(em).map(Square(_)).toVector
      Vector(PromotionMove(squares.head, squares.last, promotionresult))
    }
  }
  
  private[parsers] def parseStandardMoves(encodedMove: String): Vector[ChessMove] = {
    if (encodedMove.matches(s"S\\[$ChessRegex.doubleSquare\\]")) {
      val squares = ChessRegex.singleSquare.findAllMatchIn(encodedMove)
      .map(m => Square(m.matched)).toVector
      Vector(StandardMove(squares.head, squares.last))
    }
    else {
      val (src, targets) = parseMultiMove(encodedMove)
      targets.map(target => StandardMove(src, target))
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
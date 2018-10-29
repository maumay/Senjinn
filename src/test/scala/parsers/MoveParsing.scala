package senjinn.parsers

import senjinn.base.{Square}
import senjinn.moves.{ChessMove, StandardMove, PromotionMove, EnpassantMove, CastleMove}

/**
 * Trait which exposes a single method for parsing shorthand
 * moves for reading test cases involving lot's of moves, e.g
 * testing generation of legal moves for a certain boardstate.
 */
trait MoveParsing
{
  /**
   * Parses moves described by my shorthand notation defined
   * to more easily write test cases.
   */
  final def parseMoves(encodedMoves: String): Vector[ChessMove] = {
    val em = encodedMoves.trim.toUpperCase
    require(em.matches(ChessRegex.shorthandMove.regex))
    em.charAt(0) match {
      case 'S' => parseStandardMoves(em)
      case 'P' => parsePromotionMoves(em)
      case 'E' => parseEnpassantMove(em)
      case 'C' => parseCastlingMoves(em)
      case _   => throw new AssertionError
    }
  }
  
  private[parsers] def parseCastlingMoves(encodedMoves: String): Vector[ChessMove] = {
    ChessRegex.castleZone.findAllIn(encodedMoves).map(CastleMove(_)).toVector
  }
  
  private[parsers] def parseEnpassantMove(encodedMove: String): Vector[ChessMove] = {
    val squares = ChessRegex.singleSquare.findAllIn(encodedMove).map(Square(_)).toVector
    Vector(EnpassantMove(squares.head, squares.last))
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
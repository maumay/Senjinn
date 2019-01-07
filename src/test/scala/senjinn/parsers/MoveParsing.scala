package senjinn.parsers

import senjinn.base.{Square, Dir}
import senjinn.moves.{Move, StandardMove, PromotionMove, EnpassantMove, CastleMove}

/**
 * Trait which exposes a single method for parsing shorthand
 * moves for reading test cases involving lot's of moves, e.g
 * testing generation of legal moves for a certain boardstate.
 */
trait MoveParsing
{
  final def parseMoves(encodedMoves: Seq[String]): Vector[Move] = {
    encodedMoves.flatMap(parseMoves(_)).toVector
  }
  
  /**
   * Parses moves described by my shorthand notation defined
   * to more easily write test cases.
   */
  final def parseMoves(encodedMoves: String): Vector[Move] = {
    val em = encodedMoves.trim.toUpperCase
    ChessRegex.shorthandMove.findAllIn(em).flatMap(m => m(0) match {
      case 'S' => parseStandardMoves(em)
      case 'P' => parsePromotionMoves(em)
      case 'E' => parseEnpassantMove(em)
      case 'C' => parseCastlingMoves(em)
      case _   => throw new AssertionError
    }).toVector
  }
  
  private def parseCastlingMoves(encodedMoves: String): Vector[Move] = {
    ChessRegex.castleZone.findAllIn(encodedMoves).map(CastleMove(_)).toVector
  }
  
  private def parseEnpassantMove(encodedMove: String): Vector[Move] = {
    val squares = ChessRegex.square.findAllIn(encodedMove).map(Square(_)).toVector
    Vector(EnpassantMove(squares.head, squares.last))
  }
  
  private def parsePromotionMoves(encodedMoves: String): Vector[Move] = {
    val em = encodedMoves.trim.toUpperCase
    val promotionresult = "[NBRQ]".r.findAllIn(em).toVector.last
    val (mult, cord) = (ChessRegex.multiTarget.regex, ChessRegex.cord.regex)
    if (em.matches(s"P\\[($mult|$cord) $promotionresult\\]")) {
      val (src, targets) = parseMultiMove(em.substring(2, em.length - 3))
      targets.map(target => PromotionMove(src, target, promotionresult))
    }
    else {
      val squares = ChessRegex.square.findAllIn(em).map(Square(_)).toVector
      Vector(PromotionMove(squares.head, squares.last, promotionresult))
    }
  }
  
  private def parseStandardMoves(encodedMove: String): Vector[Move] = {
    if (encodedMove.matches(s"S\\[$ChessRegex.doubleSquare\\]")) {
      val squares = ChessRegex.square.findAllMatchIn(encodedMove)
      .map(m => Square(m.matched)).toVector
      Vector(StandardMove(squares.head, squares.last))
    }
    else {
      val (src, targets) = parseMultiMove(encodedMove)
      targets.map(target => StandardMove(src, target))
    }
  }
  
  private def parseMultiMove(encodedMoves: String): (Square, Vector[Square]) = {
    val em = encodedMoves.trim
    val cord = ChessRegex.cord.findFirstIn(em).map(parseCord(_))
    val mult = ChessRegex.multiTarget.findFirstIn(em).map(parseMultiTarget(_))
    (cord, mult) match {
      case (Some(x), None) => x
      case (None, Some(x)) => x
      case _               => throw new RuntimeException
    }
  }
  
  private def parseCord(x: String): (Square, Vector[Square]) = {
    val matchedSquares = ChessRegex.square
    .findAllMatchIn(x)
    .map {m => Square(m.matched)}
    .toVector
    val (start, end) = (matchedSquares.head, matchedSquares.last)
    val dir = Dir.ofLineConnecting(start, end)
    val span = start.allSquares(dir, 8).span(_ != end)
    (start, span._1 ++ span._2.take(1))
  }
  
  private def parseMultiTarget(x: String): (Square, Vector[Square]) = {
    val matchedSquares = ChessRegex.square
    .findAllMatchIn(x)
    .map {m => Square(m.matched)}
    .toVector
    (matchedSquares.head, matchedSquares.drop(1))
  }
}
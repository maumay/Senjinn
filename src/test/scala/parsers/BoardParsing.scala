package senjinn.parsers

import scala.collection.{ mutable => mutable }
import senjinn.base.{ Square, CastleZone, Side }
import senjinn.base.ImplicitAreaConverters.{ boardsquare2long }
import senjinn.board.{ BoardState, PieceLocations, CastlingTracker }
import senjinn.base.CastleZone

/**
 */
trait BoardParsing 
{
  final def parseBoard(attributes: Seq[String]): BoardState = {
    parseBoard(attributes, 20)
  }
  
  final def parseBoard(attributes: Seq[String], moveCount: Int): BoardState = {
    require(moveCount >= 0)
    require(attributes.size == 9)
    
    throw new RuntimeException
  }
  
  private def parsePieceLocations(whiteLocs: String, blackLocs: String): PieceLocations = {
    require(whiteLocs.matches(ChessRegex.whiteLocationsAttribute.regex))
    require(blackLocs.matches(ChessRegex.blackLocationsAttribute.regex))
    
    PieceLocations(ChessRegex.groupedSquares.findAllIn(whiteLocs + blackLocs)
      .map(ChessRegex.square.findAllIn(_).map(Square(_)))
      .map(_.foldLeft(0L)(_|_))
      .toArray)
  }

  private def parseHalfMoveClock(clock: String): Int = {
    require(clock.matches(ChessRegex.halfMoveClockAttribute.regex))
    "[0-9]+".r.findFirstIn(clock).map(Integer.parseInt(_)) match {
      case Some(x) => x
      case _       => throw new RuntimeException
    }
  }
  
  private def parseCastlingStatus(rights: String, whiteStatus: String, blackStatus: String): CastlingTracker = {
    require(rights.matches(ChessRegex.castlingRightsAttribute.regex))
    require(whiteStatus.matches(ChessRegex.whiteCastleStatusAttribute.regex))
    require(blackStatus.matches(ChessRegex.blackCastleStatusAttribute.regex))
    val rightsMatchers = CastleZone.simpleIdentifierMap
    
    val rightsMatched = rightsMatchers.keys
    .filter(_.r.findFirstIn(rights).isDefined).map(rightsMatchers(_)).toSet
    val whiteMatched = rightsMatchers.keys
    .find(_.r.findFirstIn(whiteStatus).isDefined).map(rightsMatchers(_))
    val blackMatched = rightsMatchers.keys
    .find(_.r.findFirstIn(blackStatus).isDefined).map(rightsMatchers(_))
    
    CastlingTracker(rightsMatched, whiteMatched, blackMatched)
  }
  
  private def parseDevelopedPieces(encoded: String): mutable.Set[Square] = {
    require(encoded.matches(ChessRegex.developedPiecesAttribute.regex))
    ChessRegex.square.findAllIn(encoded).map(Square(_)).to[mutable.Set]
  }
  
  private def parseActiveSide(encoded: String): Side = {
    require(encoded.matches(ChessRegex.activeSideAttribute.regex))
    val whiteMatch = "white".r.findFirstIn(encoded)
    if (whiteMatch.isDefined) Side.white else Side.black
  }
  
  
}
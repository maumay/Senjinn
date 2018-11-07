package senjinn.parsers

import scala.collection.{ mutable => mutable }
import senjinn.base.{ Square, CastleZone, Side, DevPiece }
import senjinn.base.{ boardsquare2long }
import senjinn.board.{ Board, PieceLocations, CastlingTracker, HashCache, BoardHasher }
import senjinn.base.CastleZone

/**
 */
trait BoardParsing 
{
  final def parseBoard(attributes: Seq[String], moveCount: Int): Board = {
    require(moveCount >= 0)
    require(attributes.size == 9)
    val att = attributes
    val pieceLocs = parsePieceLocations(att(0), att(1))
    val halfClock = parseHalfMoveClock(att(2))
    val castling = parseCastlingStatus(att(3), att(4), att(5))
    val developed = parseDevelopedPieces(att(6))
    val active = parseActiveSide(att(7))
    val enpassant = parseEnpassantSquare(att(8))
    val hash = pieceLocs.hash ^ BoardHasher.hashFeatures(active, enpassant, castling)
    val hashCache = constructDummyHashCache(hash, moveCount)
    
    new Board(pieceLocs, hashCache, castling,
        developed, halfClock, enpassant, active)
  }
  
  private def parsePieceLocations(whiteLocs: String, 
      blackLocs: String): PieceLocations = {
    require(whiteLocs.matches(ChessRegex.whiteLocationsAttribute.regex), whiteLocs)
    require(blackLocs.matches(ChessRegex.blackLocationsAttribute.regex), blackLocs)
    
    PieceLocations(ChessRegex.groupedSquares.findAllIn(whiteLocs + blackLocs)
      .map(ChessRegex.square.findAllIn(_).map(Square(_)))
      .map(_.foldLeft(0L)(_|_)).toArray)
  }

  private def parseHalfMoveClock(clock: String): Int = {
    require(clock.matches(ChessRegex.halfMoveClockAttribute.regex))
    "[0-9]+".r.findFirstIn(clock).map(Integer.parseInt(_)) match {
      case Some(x) => x
      case _       => throw new RuntimeException
    }
  }
  
  private def parseCastlingStatus(rights: String, whiteStatus: String, 
      blackStatus: String): CastlingTracker = {
    require(rights.matches(ChessRegex.castlingRightsAttribute.regex))
    require(whiteStatus.matches(ChessRegex.whiteCastleStatusAttribute.regex))
    require(blackStatus.matches(ChessRegex.blackCastleStatusAttribute.regex))
    val rightsMatchers: String => CastleZone = CastleZone(_)
    val possibleRights = Seq("wk", "wq", "bk", "bq")
    
    val rightsMatched = possibleRights.iterator
    .filter(_.r.findFirstIn(rights).isDefined).map(rightsMatchers(_)).toSet
    val whiteMatched = possibleRights.iterator
    .find(_.r.findFirstIn(whiteStatus).isDefined).map(rightsMatchers(_))
    val blackMatched = possibleRights.iterator
    .find(_.r.findFirstIn(blackStatus).isDefined).map(rightsMatchers(_))
    
    CastlingTracker(rightsMatched, whiteMatched, blackMatched)
  }
  
  private def parseDevelopedPieces(encoded: String): mutable.Set[DevPiece] = {
    require(encoded.matches(ChessRegex.developedPiecesAttribute.regex))
    ChessRegex.square.findAllIn(encoded)
    .map(Square(_)).map(DevPiece(_).get).to[mutable.Set]
  }
  
  private def parseActiveSide(encoded: String): Side = {
    require(encoded.matches(ChessRegex.activeSideAttribute.regex))
    val whiteMatch = "white".r.findFirstIn(encoded)
    if (whiteMatch.isDefined) Side.White else Side.Black
  }
  
  private def parseEnpassantSquare(encoded: String): Option[Square] = {
    require(encoded.matches(ChessRegex.enpassantAttribute.regex))
    ChessRegex.square.findFirstIn(encoded).map(Square(_))
  }
  
  private def constructDummyHashCache(boardHash: Long, 
      moveCount: Int): HashCache = {
    val cache = (1 to HashCache.size).map(_.toLong).toArray
    cache(moveCount % HashCache.size) = boardHash
    HashCache(cache, moveCount)
  }
}
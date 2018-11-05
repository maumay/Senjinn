package senjinn.parsers
import scala.util.matching.Regex

/**
 */
object ChessRegex
{
  val square: Regex = "([a-hA-H][1-8])".r
  private def sq = square.regex
  
  // Parsing moves
  private val arrow: String = "\\-\\>"
  val doubleSquare: Regex = s"($square +$square)".r
  val cord: Regex = s"($sq$arrow$sq)".r
  val multiTarget: Regex = s"($sq$arrow\\{( *$square *)+\\})".r
  val shorthandMove: Regex = raw"([sScCeEpP]\[[a-hA-H1-8kKqQwW \-\>\{\}]+( [NBRG])?\])".r
  val castleZone = "(([wW][kK])|([wW][qQ])|([bB][kK])|([bB][qQ]))".r
  
  // Board state parsing regex
  val enpassantAttribute = s"^enpassant_square: *(none|$sq)$$".r
  val activeSideAttribute = "^active_side: *(white|black)$".r
  val developedPiecesAttribute = s"^developed_pieces:( *none| *$sq( +$sq)*)$$".r
  val castlingRightsAttribute = "^castling_rights:( *wk)?( +wq)?( +bk)?( +bq)?$".r
  val whiteCastleStatusAttribute = "^white_castle_status: *(none|wk|wq)$".r
  val blackCastleStatusAttribute = "^black_castle_status: *(none|bk|bq)$".r
  val halfMoveClockAttribute = "^half_move_clock: *[0-9]+$".r
  
  val groupedSquares = s"\\( *($sq *)?( +$sq)* *\\)".r
  val sixGroups = (1 to 6).map(_ => groupedSquares.regex).foldLeft(" *")(_ + " +" + _).r
  val whiteLocationsAttribute = s"^white_pieces:$sixGroups.regex$$".r
  val blackLocationsAttribute = s"^black_pieces:$sixGroups.regex$$".r
}

package senjinn.board

import scala.collection.{mutable => mutable}
import senjinn.base.{DevPiece, Side, Square, CastleZone, SquareSet, Piece}


/**
  * A mutable representation of the state of a chessboard, it
  * is designed so that a move can mutate it in the forward 
  * direction and reverse it using an appropriate instance of
  * {@link MoveReverser}.  
  */
class Board(
  val pieceLocations: PieceLocations,
  val hashCache: HashCache,
  val castleStatus: CastlingTracker,
  val piecesDeveloped: mutable.Set[DevPiece],
  var clock: Int,
  var enpassant: Option[Square],
  private var _active: Side)
{
  def active = _active
  
  def passive = Side.other(active)
  
  def switchActive() {_active = Side.other(_active) }
  
  def computeHash = {
    pieceLocations.hash ^ BoardHasher.hashFeatures(active, enpassant, castleStatus)
  }
  
  def copy: Board = new Board(pieceLocations.copy,
		  hashCache.copy, castleStatus.copy, piecesDeveloped.to[mutable.Set],
		  clock, enpassant, _active)
  
  // Object API
  override def equals(x: Any) = {
    x.isInstanceOf[Board] && {
      val o = x.asInstanceOf[Board]
      fieldTuple == o.fieldTuple
    }
  }
  
  override def hashCode(): Int = fieldTuple.##
  
  private def fieldTuple = (pieceLocations, hashCache, 
      castleStatus, piecesDeveloped, clock, enpassant, active)
}

object Board
{
    def apply(properties: Map[String, Any]): Board = {
      val p = properties
      new Board(
          p("pieceLocations").asInstanceOf[PieceLocations],
          p("hashCache").asInstanceOf[HashCache],
          p("castleStatus").asInstanceOf[CastlingTracker],
          p("piecesDeveloped").asInstanceOf[mutable.Set[DevPiece]],
          p("clock").asInstanceOf[Int],
          p("enpassant").asInstanceOf[Option[Square]],
          p("active").asInstanceOf[Side]
      )
    }
}

package xawd.senjinn.board

import xawd.senjinn.{BoardSquare, CastleZone, Side}
import xawd.senjinn.pieces.ChessPiece


object BoardHasher 
{
  private type Arr       = Array[Long]
  private type SquareArr = Array[Arr]
  
  private val prng = new java.util.Random(0x110894L)
  private val genLong: Any => Long = _ => prng.nextLong()
  
  private val squareFeatures: SquareArr = {
    ChessPiece.all.map(i => BoardSquare.values.map(genLong).toArray).toArray
  }
  
  private val castleFeatures: Arr = CastleZone.values.map(genLong).toArray
  private val enpassantFeatures: Arr = (1 to 8).map(genLong).toArray
  
  // Api
  def squareFeature(piece: ChessPiece, square: BoardSquare) = squareFeatures(piece.index)(square.index)
  def castleFeature(zone: CastleZone) = castleFeatures(zone.index)
  def enpassantFeature(enpassantSquare: BoardSquare) = enpassantFeatures(enpassantSquare.file)
  val blackMoveFeature: Long = genLong(Unit)

  def hashFeatures(active: Side, enpassant: Option[BoardSquare], castling: CastlingTracker) = {
    val activehash = if (active.isWhite) 0L else blackMoveFeature
    val enpassantHash = enpassant.map(enpassantFeature(_)).getOrElse(0L)
    val castlingHash = castling.rights.map(castleFeature(_)).foldLeft(0L)(_ ^ _)
    activehash ^ enpassantHash ^ castlingHash
  }
}

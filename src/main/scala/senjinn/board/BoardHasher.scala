package senjinn.board

import senjinn.base.{Square, CastleZone, Side, ChessPiece}


object BoardHasher 
{
  /* Implementation details */
  private type Arr       = Array[Long]
  private type SquareArr = Array[Arr]
  
  private val prng = new java.util.Random(0x110894L)
  private val genLong: Any => Long = _ => prng.nextLong()
  
  private val squareFeatures: SquareArr = {
    ChessPiece.values.map(i => Square.values.map(genLong).toArray).toArray
  }
  
  private val castleFeatures: Arr = CastleZone.values.map(genLong).toArray
  private val enpassantFeatures: Arr = (1 to 8).map(genLong).toArray
  
  // Api
  def squareFeature(piece: ChessPiece, square: Square) = squareFeatures(piece.index)(square.index)
  
  def castleFeature(zone: CastleZone) = castleFeatures(CastleZone.indexOf(zone))
  
  def enpassantFeature(enpassantSquare: Square) = enpassantFeatures(enpassantSquare.file)
  
  val blackMoveFeature: Long = genLong(Unit)

  def hashFeatures(active: Side, enpassant: Option[Square], castling: CastlingTracker) = {
    val activehash = if (active.isWhite) 0L else blackMoveFeature
    val enpassantHash = enpassant.map(enpassantFeature(_)).getOrElse(0L)
    val castlingHash = castling.rights.map(castleFeature(_)).foldLeft(0L)(_ ^ _)
    activehash ^ enpassantHash ^ castlingHash
  }
}

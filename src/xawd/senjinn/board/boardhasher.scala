package xawd.senjinn.board

import xawd.senjinn.BoardSquare
import xawd.senjinn.ChessPiece
import xawd.senjinn.CastleZone


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
  def castleFeature(piece: ChessPiece, square: BoardSquare) = squareFeatures(piece.index)(square.index)
  def enpassantFeature(enpassantSquare: BoardSquare) = enpassantFeatures(enpassantSquare.file)
  val blackMoveFeature: Long = genLong(Unit)
}
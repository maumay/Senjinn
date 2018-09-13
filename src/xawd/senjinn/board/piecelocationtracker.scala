package xawd.senjinn.board

import xawd.senjinn.BoardSquare
import xawd.senjinn.SquareSet
import xawd.senjinn.ChessPiece



class PieceLocations private(private val locs: Array[Long]) extends Iterable[SquareSet]
{
  require(locs.length == 12)
  
  def locs(piece: ChessPiece): SquareSet = locs(piece.index)
  
  def contains(piece: ChessPiece, loc: BoardSquare): Boolean = locs(piece).intersects(loc)
  
  def pieceCount(piece: ChessPiece): Int = java.lang.Long.bitCount(locs(piece.index))
  
  def addSquare(piece: ChessPiece, square: BoardSquare) { 
    assert(!locs(piece).intersects(square))
    locs(piece.index) |= square.loc
  }
  
  def removeSquare(piece: ChessPiece, square: BoardSquare) {
    assert(locs(piece).intersects(square))
    locs(piece.index) ^= square.loc
  }
  
  def iterator = locs.iterator.map(x => x: SquareSet)
}

object PieceLocations
{
//  def evaluateLocations(tables: Piece
}
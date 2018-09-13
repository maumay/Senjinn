package xawd.senjinn.board

import xawd.senjinn.BoardSquare
import xawd.senjinn.SquareSet


class MutableSquareSet(private var _locs: SquareSet) extends Iterable[BoardSquare]
{
  def locs = _locs
  
  def contains(square: BoardSquare): Boolean = _locs.intersects(square)
  
  def pieceCount = java.lang.Long.bitCount(_locs.src)
  
  def addSquare(square: BoardSquare) { assert(!_locs.intersects(square)); _locs |= square }
  
  def removeSquare(square: BoardSquare) { assert(_locs.intersects(square)); _locs ^= square }
  
  override def iterator: Iterator[BoardSquare] = _locs.squares
  
  // Need to do hashcode / equals etc.
}


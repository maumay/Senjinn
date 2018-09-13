package xawd.senjinn.board

import xawd.senjinn.BoardSquare
import xawd.senjinn.SquareSet
import xawd.senjinn.ChessPiece
import xawd.senjinn.eval.PieceSquareTableSet



class PieceLocations private(private val locs: Array[Long]) extends Iterable[SquareSet]
{
  import xawd.senjinn.SquareSet.{long2squareset}
  require(locs.length == 12)
  
  private var _midgameEval: Int = {
    val squareLocs = locs.map(loc => loc.squares)
    val tables = PieceSquareTableSet.midgame
    ChessPiece.all.zip(squareLocs).map(p => p._2.foldLeft(0)((n, sq) => n + tables.value(p._1, sq))).reduce(_ + _)
  }
  
  private var _endgameEval: Int = {
    val squareLocs = locs.map(loc => loc.squares)
    val tables = PieceSquareTableSet.endgame
    ChessPiece.all.zip(squareLocs).map(p => p._2.foldLeft(0)((n, sq) => n + tables.value(p._1, sq))).reduce(_ + _)
  }
  
  private var _whites: SquareSet = ChessPiece.white.map(p => locs(p.index)).reduce(_ | _)
  private var _blacks: SquareSet = ChessPiece.black.map(p => locs(p.index)).reduce(_ | _)
  
  
  def locs(piece: ChessPiece): SquareSet = {
    locs(piece.index)
  }
  
  def contains(piece: ChessPiece, loc: BoardSquare): Boolean = {
    locs(piece).intersects(loc)
  }
  
  def pieceCount(piece: ChessPiece): Int = {
    java.lang.Long.bitCount(locs(piece.index))
  }
  
  def addSquare(piece: ChessPiece, square: BoardSquare) { 
    assert(!locs(piece).intersects(square))
    locs(piece.index) |= square.loc
    if (piece.side.isWhite) _whites |= square.loc else _blacks |= square.loc
    _midgameEval += PieceSquareTableSet.midgame.value(piece, square)
    _endgameEval += PieceSquareTableSet.endgame.value(piece, square)
  }
  
  def removeSquare(piece: ChessPiece, square: BoardSquare) {
    assert(locs(piece).intersects(square))
    locs(piece.index) ^= square.loc
    if (piece.side.isWhite) _whites ^= square.loc else _blacks ^= square.loc
    _midgameEval -= PieceSquareTableSet.midgame.value(piece, square)
    _endgameEval -= PieceSquareTableSet.endgame.value(piece, square)
  }
  
  def iterator = locs.iterator.map(x => x: SquareSet)
  
  // getters
  def midgameEval = _midgameEval
  def endgameEval = _endgameEval
  def whites = _whites
  def blacks = _blacks
}

object PieceLocations
{
//  def evaluateLocations(tables: Piece
}

//  def midgameEvaluation(locs: PieceLocations): Int = {
//    midgame.zip(locs).map(p => p._2.squares.foldLeft(0)((n, sq) => n + p._1.valueAt(sq))).reduce(_ + _)
//  }
//  
//  def endgameEvaluation(locs: PieceLocations): Int = {
//    endgame.zip(locs).map(p => p._2.squares.foldLeft(0)((n, sq) => n + p._1.valueAt(sq))).reduce(_ + _)
//  }
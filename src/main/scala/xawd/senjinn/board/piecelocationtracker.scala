package xawd.senjinn.board

import xawd.senjinn.BoardSquare
import xawd.senjinn.SquareSet
import xawd.senjinn.ChessPiece
import xawd.senjinn.eval.PieceSquareTableSet
import xawd.senjinn.ImplicitAreaConverters._


/**
 * This is a mutable class designed to track the locations of all pieces.
 */
class PieceLocations private(private val _locs: Array[Long]) extends Iterable[SquareSet]
{
  require(_locs.length == 12)
  
  /** Self-updating hash value of all the piece-square features. */
  private var _positionHash: Long = {
    val square_locs = _locs.map(_.squares)
    val sf = BoardHasher.squareFeature(_, _)
    ChessPiece.all.zip(square_locs).map(p => p._2.foldLeft(0L)( _ ^ sf(p._1, _))).reduce(_ ^ _)
  }
  
  /** Self-updating evaluation of the piece locations using the midgame tables. */
  private var _midgameEval: Int = {
    val square_locs = _locs.map(_.squares)
    val tv = PieceSquareTableSet.midgame.value(_, _)
    ChessPiece.all.zip(square_locs).map(p => p._2.foldLeft(0)(_ + tv(p._1, _))).reduce(_ + _)
  }
  
  /** Self-updating evaluation of the piece locations using the endgame tables. */
  private var _endgameEval: Int = {
    val square_locs = _locs.map(_.squares)
    val tv = PieceSquareTableSet.endgame.value(_, _)
    ChessPiece.all.zip(square_locs).map(p => p._2.foldLeft(0)(_ + tv(p._1, _))).reduce(_ + _)
  }
  
  /** Self-updating set tracking location of all white pieces. */
  private var _whites: SquareSet = ChessPiece.white.map(p => _locs(p.index)).reduce(_ | _)

  /** Self-updating set tracking location of all black pieces. */
  private var _blacks: SquareSet = ChessPiece.black.map(p => _locs(p.index)).reduce(_ | _)
  
  
  def contains(piece: ChessPiece, loc: BoardSquare): Boolean = {
    locs(piece).intersects(loc)
  }
  
  def pieceCount(piece: ChessPiece): Int = {
    java.lang.Long.bitCount(locs(piece))
  }
  
  def addSquare(piece: ChessPiece, square: BoardSquare) { 
    assert(!locs(piece).intersects(square))
    _locs(piece.index) |= square.loc
    if (piece.side.isWhite) _whites |= square.loc else _blacks |= square.loc
    _midgameEval += PieceSquareTableSet.midgame.value(piece, square)
    _endgameEval += PieceSquareTableSet.endgame.value(piece, square)
    _positionHash ^= BoardHasher.squareFeature(piece, square)
  }
  
  def removeSquare(piece: ChessPiece, square: BoardSquare) {
    assert(locs(piece).intersects(square))
    _locs(piece.index) ^= square.loc
    if (piece.side.isWhite) _whites ^= square.loc else _blacks ^= square.loc
    _midgameEval -= PieceSquareTableSet.midgame.value(piece, square)
    _endgameEval -= PieceSquareTableSet.endgame.value(piece, square)
    _positionHash ^= BoardHasher.squareFeature(piece, square)
  }
  
  def iterator = _locs.iterator.map(x => x: SquareSet)
  
  // getters
  def locs(piece: ChessPiece): SquareSet = _locs(piece.index)
  def positionHash = _positionHash
  def midgameEval = _midgameEval
  def endgameEval = _endgameEval
  def whites = _whites
  def blacks = _blacks
}

object PieceLocations
{
  def apply(locs: Map[ChessPiece, Set[BoardSquare]]): PieceLocations = {
    val xs = ChessPiece.all.map(locs.getOrElse(_, Set()).foldLeft(0L)((n, sq) => n | sq.loc)).toArray
    val x = Set()
    null
  }
}

//  def midgameEvaluation(locs: PieceLocations): Int = {
//    midgame.zip(locs).map(p => p._2.squares.foldLeft(0)((n, sq) => n + p._1.valueAt(sq))).reduce(_ + _)
//  }
//  
//  def endgameEvaluation(locs: PieceLocations): Int = {
//    endgame.zip(locs).map(p => p._2.squares.foldLeft(0)((n, sq) => n + p._1.valueAt(sq))).reduce(_ + _)
//  }
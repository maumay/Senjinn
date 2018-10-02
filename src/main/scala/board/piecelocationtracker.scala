package senjinn.board

import senjinn.base.{BoardSquare, SquareSet, Side}
import senjinn.base.ImplicitAreaConverters._
import senjinn.pieces.ChessPiece
import senjinn.eval.PieceSquareTableSet


/**
 * This is a mutable class designed to track the locations of all pieces.
 */
class PieceLocations private(private val _locs: Array[Long]) extends Iterable[SquareSet]
{
  require(_locs.length == 12)
  
  /** Self-updating hash value of all the piece-square features. */
  private var _hash: Long = {
    val allLocs = _locs.map(_.squares)
    val sf = BoardHasher.squareFeature(_, _)
    ChessPiece.all.zip(allLocs).map(p => p._2.foldLeft(0L)( _ ^ sf(p._1, _))).reduce(_^_)
  }
  
  /** Self-updating evaluation of the piece locations using the midgame tables. */
  private var _midgameEval: Int = {
    val allLocs = _locs.map(_.squares)
    val tv = PieceSquareTableSet.midgame.value(_, _)
    ChessPiece.all.zip(allLocs).map(p => p._2.foldLeft(0)(_ + tv(p._1, _))).reduce(_+_)
  }
  
  /** Self-updating evaluation of the piece locations using the endgame tables. */
  private var _endgameEval: Int = {
    val allLocs = _locs.map(_.squares)
    val tv = PieceSquareTableSet.endgame.value(_, _)
    ChessPiece.all.zip(allLocs).map(p => p._2.foldLeft(0)(_ + tv(p._1, _))).reduce(_+_)
  }
  
  /** Self-updating set tracking location of all white pieces. */
  private var _whites: SquareSet = ChessPiece.whites.map(p => _locs(p.index)).reduce(_|_)

  /** Self-updating set tracking location of all black pieces. */
  private var _blacks: SquareSet = ChessPiece.blacks.map(p => _locs(p.index)).reduce(_|_)
  
  
  def contains(piece: ChessPiece, loc: BoardSquare): Boolean = {
    locs(piece).intersects(loc)
  }

  def pieceAt(square: BoardSquare, side: Side) = {
    ChessPiece(side).find(contains(_, square))
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
    _hash ^= BoardHasher.squareFeature(piece, square)
  }
  
  def removeSquare(piece: ChessPiece, square: BoardSquare) {
    assert(locs(piece).intersects(square))
    _locs(piece.index) ^= square.loc
    if (piece.side.isWhite) _whites ^= square.loc else _blacks ^= square.loc
    _midgameEval -= PieceSquareTableSet.midgame.value(piece, square)
    _endgameEval -= PieceSquareTableSet.endgame.value(piece, square)
    _hash ^= BoardHasher.squareFeature(piece, square)
  }
  
  def iterator = _locs.iterator.map(x => x: SquareSet)
  
  // getters
  def locs(piece: ChessPiece): SquareSet = _locs(piece.index)
  def hash = _hash
  def midgameEval = _midgameEval
  def endgameEval = _endgameEval
  def whites = _whites
  def blacks = _blacks

  // Equality methods -- Maybe these should be 'static' in testing code?
  override def equals(other: Object) = {
    other.isInstanceOf[PieceLocations] && {
      val that = other.asInstanceOf[PieceLocations]
      _locs.toList == that._locs.toList &&
      hash == that.hash &&
      midgameEval == that.midgameEval &&
      endgameEval == that.endgameEval &&
      whites == that.whites &&
      blacks == that.blacks
    }
  }

  override def hashCode = {
    (_locs.toList, hash, midgameEval, endgameEval, whites, blacks).##
  }
}

object PieceLocations
{
  def apply(locs: Map[ChessPiece, Set[BoardSquare]]): PieceLocations = {
    // val xs = ChessPiece.all.map(locs.getOrElse(_, Set()).foldLeft(0L)(_|_)).toArray
    // val x = Set()
    // null
    throw new RuntimeException
  }
}

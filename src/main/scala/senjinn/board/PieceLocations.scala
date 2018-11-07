package senjinn.board

import senjinn.base.{Square, SquareSet, Side, Piece, long2squareset}
import senjinn.eval.PieceSquareTableSet


/**
  * This is a mutable class designed to track the locations of all pieces.
  */
class PieceLocations private(private val _locs: Array[Long]) extends Iterable[SquareSet]
{
  // Instance variables
  /** Self-updating hash value of all the piece-square features. */
  private var _hash: Long = {
    val allLocs = _locs.map(_.squares)
    val sf = BoardHasher.squareFeature(_, _)
    Piece.values.zip(allLocs).map(p => p._2.foldLeft(0L)( _ ^ sf(p._1, _))).reduce(_^_)
  }
  
  /** Self-updating evaluation of the piece locations using the midgame tables. */
  private var _midgameEval: Int = {
    val allLocs = _locs.map(_.squares)
    val tv = PieceSquareTableSet.midgame.value(_, _)
    Piece.values.zip(allLocs).map(p => p._2.foldLeft(0)(_ + tv(p._1, _))).reduce(_+_)
  }
  
  /** Self-updating evaluation of the piece locations using the endgame tables. */
  private var _endgameEval: Int = {
    val allLocs = _locs.map(_.squares)
    val tv = PieceSquareTableSet.endgame.value(_, _)
    Piece.values.zip(allLocs).map(p => p._2.foldLeft(0)(_ + tv(p._1, _))).reduce(_+_)
  }
  
  /** Self-updating set tracking location of all white pieces. */
  private var _whites: SquareSet = Piece.whites.map(p => _locs(p.index)).reduce(_|_)

  /** Self-updating set tracking location of all black pieces. */
  private var _blacks: SquareSet = Piece.blacks.map(p => _locs(p.index)).reduce(_|_)
  
  
  // Instance methods
  def contains(piece: Piece, loc: Square): Boolean = {
    locs(piece).intersects(loc)
  }

  def pieceAt(square: Square, side: Side) = {
    Piece(side).find(contains(_, square))
  }
  
  def pieceCount(piece: Piece): Int = {
    java.lang.Long.bitCount(locs(piece))
  }
  
  def addSquare(piece: Piece, square: Square) {
    assert(!locs(piece).intersects(square))
    _locs(piece.index) |= square.loc
    if (piece.side.isWhite) _whites |= square.loc else _blacks |= square.loc
    _midgameEval += PieceSquareTableSet.midgame.value(piece, square)
    _endgameEval += PieceSquareTableSet.endgame.value(piece, square)
    _hash ^= BoardHasher.squareFeature(piece, square)
  }
  
  def removeSquare(piece: Piece, square: Square) {
    assert(locs(piece).intersects(square))
    _locs(piece.index) ^= square.loc
    if (piece.side.isWhite) _whites ^= square.loc else _blacks ^= square.loc
    _midgameEval -= PieceSquareTableSet.midgame.value(piece, square)
    _endgameEval -= PieceSquareTableSet.endgame.value(piece, square)
    _hash ^= BoardHasher.squareFeature(piece, square)
  }
  
  def iterator = _locs.iterator.map(x => x: SquareSet)
  
  // getters
  def locs(piece: Piece): SquareSet = _locs(piece.index)
  def hash = _hash
  def midgameEval = _midgameEval
  def endgameEval = _endgameEval
  def whites = _whites
  def blacks = _blacks
  
  def copy = new PieceLocations(_locs.clone())

  // Object API
  override def equals(x: Any) = {
    x.isInstanceOf[PieceLocations] && {
      val o = x.asInstanceOf[PieceLocations]
      toFieldTuple == o.toFieldTuple
    }
  }

  override def hashCode = {
    toFieldTuple.##
  }

  private def toFieldTuple = {
    (_locs.toList, hash, midgameEval, endgameEval, whites, blacks)
  }
}

object PieceLocations
{
  def apply(locs: Array[Long]): PieceLocations = {
    require(locs.length == 12)
    new PieceLocations(locs)
  }
  
  def apply(locs: Map[Piece, Set[Square]]): PieceLocations = {
    // val xs = ChessPiece.all.map(locs.getOrElse(_, Set()).foldLeft(0L)(_|_)).toArray
    // val x = Set()
    // null
    throw new RuntimeException
  }
}

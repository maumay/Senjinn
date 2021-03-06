package senjinn.base

import org.scalatest.FlatSpec

import senjinn.base.Piece._

class PieceIndexAlignmentTest extends FlatSpec
{
  "Piece indices" must "match the range (0 to 11)" in {
    assert(Piece.values.map(_.index) == (0 to 11).toVector)
  }

  "White piece ordering" must "be pnbrqk" in {
    val expectedWhiteOrder = Vector(WhitePawn, WhiteKnight, WhiteBishop, WhiteRook, WhiteQueen, WhiteKing)
    assert(expectedWhiteOrder == Piece.whites)
    assert(expectedWhiteOrder.map(_.index) == (0 to 5).toVector)
  }

  "Black piece ordering" must "be pnbrqk" in {
    val expectedBlackOrder = Vector(BlackPawn, BlackKnight, BlackBishop, BlackRook, BlackQueen, BlackKing)
    assert(expectedBlackOrder == Piece.blacks)
    assert(expectedBlackOrder.map(_.index) == (6 to 11).toVector)
  }
}
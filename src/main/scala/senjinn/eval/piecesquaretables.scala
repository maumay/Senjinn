package senjinn.eval

import senjinn.base.{Square, Piece}
import senjinn.board.PieceLocations


class PieceValues private (private val values: Array[Int]) extends Iterable[Int]
{
   def valueOf(piece: Piece): Int = values(piece.index % 6)

   def iterator = values.iterator
}

object PieceValues
{
   val midgame = PieceValues(100, 310, 320, 480, 910, 0)
   val endgame = PieceValues(120, 270, 340, 550, 940, 0)

   private def apply(xs: Int*): PieceValues = {
     require(xs.length == 6)
     require(xs.forall(_ >= 0))
     new PieceValues(xs.toArray.map(x => (x * 3.5).toInt))
   }
}


class PieceSquareTableSet private(private val tables: Vector[PieceSquareTable])
{
   require(tables.length == 12)

   def value(piece: Piece, location: Square): Int = {
     tables(piece.index).valueAt(location)
   }
}

object PieceSquareTableSet
{
   import senjinn.eval.PieceSquareTable.{parse}
   import senjinn.base.{loadResource, Side}

   private def pkg = getClass.getPackage
   private def midgameLocators = Piece.whites.map(p => (pkg, p.shortName + "-midgame"))
   private def endgameLocators = Piece.whites.map(p => (pkg, p.shortName + "-endgame"))

   val midgame: PieceSquareTableSet = {
     val white = PieceValues.midgame
       .zip(midgameLocators)
       .map(p => parse(p._1, loadResource(p._2)))
     new PieceSquareTableSet((white ++ white.map(_.invert)).toVector)
   }

   val endgame: PieceSquareTableSet = {
     val white = PieceValues.endgame
       .zip(midgameLocators)
       .map(p => parse(p._1, loadResource(p._2)))
     new PieceSquareTableSet((white ++ white.map(_.invert)).toVector)
   }
}


private class PieceSquareTable private (private val values: Array[Int])
{
   require(values.length == 64)

   def valueAt(square: Square): Int = values(square.index)

   def invert: PieceSquareTable = {
     val indexReverser: Int => Int = i => 63 - 8 * (i / 8) - (7 - (i % 8))
     new PieceSquareTable((0 until 64).map(i => -values(indexReverser(i))).toArray)
   }
}

private object PieceSquareTable
{
   def apply(pieceValue: Int, locationValues: Iterable[Int]): PieceSquareTable = {
     new PieceSquareTable(locationValues.iterator.map(_ + pieceValue).toArray)
   }
  
   def parse(piecevalue: Int, lines: Vector[String]): PieceSquareTable = {
     require(lines.length == 8)
     val np = "-?[0-9]+".r
     val intExtractor = (line: String) => np.findAllMatchIn(line).map(_.group(0).toInt).toVector
     val parsedLines = lines.reverseMap(intExtractor).flatMap(_.reverse)
     PieceSquareTable(piecevalue, parsedLines)
   }
}

package senjinn.base

import org.scalatest.FlatSpec
import senjinn.base.BoardSquare._

class SquareSetTest extends FlatSpec
{
  // Test iterator generation for squaresets
  Seq(Seq(a1, d4, b4), Seq(), Seq(g7, d2)).foreach(squareset => {
    s"The set $squareset" must "create a consistent iterator" in {
      assert(SquareSet(squareset: _*).squares.toSet == squareset.toSet)
    }
  })

  Map(Set(h3, f4) -> setBits(16, 26), Set() -> 0L, Set(b2, g7, c4) -> setBits(14, 49, 29))
    .foreach({p =>
      val (sqs, expectedsqs) = p
      s"The set $sqs" must s"match $expectedsqs" in {
        assert(SquareSet(sqs.toSeq: _*).src == expectedsqs)
      }
    })

  def setBits(indices: Int*) = {
    indices.foreach(i => require(0 <= i && i < 64))
    indices.foldLeft(0L)((x, i) => x | (1L << i))
  }
}

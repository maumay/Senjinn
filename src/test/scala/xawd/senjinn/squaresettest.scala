import org.scalatest.FlatSpec
import xawd.senjinn.BoardSquare._
import xawd.senjinn.SquareSet

class SquareSetTest extends FlatSpec
{
  // Test iterator generation for squaresets
  Seq(Seq(a1, d4, b4), Seq(), Seq(g7, d2)).foreach(squareset => {
    s"The set $squareset" must "create a consistent iterator" in {
      assert(SquareSet(squareset: _*).squares.toSet == squareset.toSet)
    }
  })
}

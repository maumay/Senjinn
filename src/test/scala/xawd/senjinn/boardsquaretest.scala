package xawd.senjinn

import org.scalatest.FlatSpec
import xawd.senjinn.BoardSquare._


class BoardSquareTest extends FlatSpec 
{
	Vector(Direction.se, Direction.s, Direction.sw, Direction.w, Direction.nw)
      .foreach(dir => {
        "Number of squares " + dir + " of A1" should "be zero" in {
		    assert(a1.squaresLeft(dir) == 0)
	    }
      })
	
}


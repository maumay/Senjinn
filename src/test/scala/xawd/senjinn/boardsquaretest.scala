package xawd.senjinn

import org.scalatest.FlatSpec
import xawd.senjinn.BoardSquare._


class BoardSquareTest extends FlatSpec 
{
	
	
	"Number of squares south-east of A1" should "be zero" in {
		assert(a1.squaresLeft(Direction.se) == 0)
	}

	"Number of squares south of A1" should "be zero" in {
		assert(a1.squaresLeft(Direction.s) == 0)
	}
}

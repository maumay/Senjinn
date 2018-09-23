package xawd.senjinn

import org.scalatest.FlatSpec
import xawd.senjinn.BoardSquare._


class BoardSquareTest extends FlatSpec 
{
  Seq(Dir.se, Dir.s, Dir.sw, Dir.w, Dir.nw, Dir.nww, Dir.nnw,
    Dir.sww, Dir.ssw, Dir.sse, Dir.see)
    .foreach(dir => {
      s"Number of squares $dir of A1" must "be zero" in {
		assert(a1.squaresLeft(dir) == 0)
	  }
    })
	
  Seq(Dir.n, Dir.e, Dir.ne)
    .foreach(dir => {
      s"Number of squares $dir of A1" must "be seven" in {
        assert(a1.squaresLeft(dir) == 7)
	  }
    })

  Seq(Dir.nne, Dir.nee)
    .foreach(dir => {
      s"Number of squares $dir of A1" must "be three" in {
        assert(a1.squaresLeft(dir) == 3)
	  }
    })
      // Seq(Dir.w, Dir.se)
      // .foreach(dir => {
      //   s"Number of squares $dir of A1" must "be three" in {
      //     assert(a1.squaresLeft(dir) == 3)
      //   }
      // })

}


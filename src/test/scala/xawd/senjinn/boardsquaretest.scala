package xawd.senjinn

import org.scalatest.FlatSpec
import xawd.senjinn.BoardSquare._


class BoardSquareTest extends FlatSpec
{
  /*
   * Tests for counting how many squares left
   * in a given direction.
   */

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

  Seq(Dir.w, Dir.s, Dir.sw)
    .foreach(dir => {
      s"Number of squares $dir of D4" must "be three" in {
        assert(d4.squaresLeft(dir) == 3)
      }
    })

  Seq(Dir.sse, Dir.ssw, Dir.sww, Dir.nww)
    .foreach(dir => {
      s"Number of squares $dir of D4" must "be one" in {
        assert(d4.squaresLeft(dir) == 1)
      }
    })

  Seq(Dir.n, Dir.e, Dir.ne)
    .foreach(dir => {
      s"Number of squares $dir of D4" must "be four" in {
        assert(d4.squaresLeft(dir) == 4)
      }
    })

  Seq(Dir.nne, Dir.nnw, Dir.nee, Dir.see)
    .foreach(dir => {
      s"Number of squares $dir of D4" must "be two" in {
        assert(d4.squaresLeft(dir) == 2)
      }
    })

  /*
   * Tests for retrieving the adjacent square in
   * a given direction.
   */
  // f8
  Map(Dir.nww -> None,
    Dir.nnw -> None,
    Dir.nw -> None,
    Dir.n -> None,
    Dir.ne -> None,
    Dir.nne -> None,
    Dir.nee -> None,
    Dir.e -> Some(g8),
    Dir.se -> Some(g7),
    Dir.see -> Some(h7),
    Dir.sse -> Some(g6),
    Dir.s -> Some(f7),
    Dir.ssw -> Some(e6),
    Dir.sww -> Some(d7),
    Dir.sw -> Some(e7),
    Dir.w -> Some(e8)).foreach(pair => {
      val (dir, target) = pair
      s"The next square $dir of f8" must s"be $target" in {
        assert(f8.nextSquare(dir) == target)
      }
    })

  // c3
  Map(Dir.nww -> Some(a4),
    Dir.nnw -> Some(b5),
    Dir.nw -> Some(b4),
    Dir.n -> Some(c4),
    Dir.ne -> Some(d4),
    Dir.nne -> Some(d5),
    Dir.nee -> Some(e4),
    Dir.e -> Some(d3),
    Dir.se -> Some(d2),
    Dir.see -> Some(e2),
    Dir.sse -> Some(d1),
    Dir.s -> Some(c2),
    Dir.ssw -> Some(b1),
    Dir.sww -> Some(a2),
    Dir.sw -> Some(b2),
    Dir.w -> Some(b3)).foreach(pair => {
      val (dir, target) = pair
      s"The next square $dir of f8" must s"be $target" in {
        assert(c3.nextSquare(dir) == target)
      }
    })

  Seq((c3, Seq(Dir.nnw, Dir.se), None: Option[Int], Set(b5, a7, d2, e1))
  ).foreach(quad => {
    val (src, dirs, prox, res) = quad
    s"The squares $dirs of $src within a proximity of $prox" must s"be $res" in {
      if (prox.isDefined) {
        assert(src.allSquares(dirs, prox.get).toSet == res)
      } else {
        assert(src.allSquares(dirs).toSet == res)
      }
    }
  })
}


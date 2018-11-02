package moves

import org.scalatest.FlatSpec
import senjinn.parsers.MoveParsing
import senjinn.parsers.BoardParsing
import senjinn.base.{loadResource}
import senjinn.board.{ BoardState }
import senjinn.moves.{ ChessMove, StandardMove, CastleMove, EnpassantMove, PromotionMove }

/**
 */
class EvolutionTest extends FlatSpec with MoveParsing with BoardParsing
{
  val testpkg: Package = getClass.getPackage
  type TestCaseArgs = (ChessMove, BoardState, BoardState)

  //  def testCaseIterator: Iterator[TestCaseArgs] = {
  //    val zeropad = (n: Int) => ('0' * n.toString.length - 1) + n.toString
  //    (0 until 40) map { zeropad }
  //    map { name => loadResource( testpkg, name) }
  //    map { lines => (parseMove(lines()
  //  }
  //

  private def parseMove(encoded: String): ChessMove = {
    import senjinn.parsers.ChessRegex.{ castleZone => czregex }
    val (src, target) = "[a-h][1-8]".r.findAllIn(encoded).toVector.splitAt(1)
    encoded.toLowerCase.head match {
      case 's' => StandardMove(src(0), target(0))
      case 'e' => EnpassantMove(src(0), target(0))
      case 'c' => CastleMove(czregex.findFirstIn(encoded).get)
      case 'p' => PromotionMove(src(0), target(0), "[nbrq] ".r.findFirstIn(encoded).get)
      case _   => throw new RuntimeException
    }
  }
}
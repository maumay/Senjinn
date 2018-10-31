package parsers

import org.scalatest.FlatSpec
import senjinn.parsers.MoveParsing
import senjinn.moves.{ChessMove, StandardMove, CastleMove, EnpassantMove, PromotionMove}
import senjinn.parsers.ChessRegex

/**
 */
class MoveParsingTest extends FlatSpec with MoveParsing
{
  val validParseCases = Map[String, Set[ChessMove]](
      "s[a1->a3]" -> Set(StandardMove("a1", "a2"), StandardMove("a1", "a3")),
      "S[a1->{ a2 a3  }]" -> Set(StandardMove("a1", "a2"), StandardMove("a1", "a3"))
      )
      
  validParseCases.iterator foreach { pair =>
    val (encoded, expected) = pair
    s"$encoded" must s"parse to $expected" in {
      assert(expected == parseMoves(encoded).toSet)
    }
  }
}
package senjinn.parsers

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
      "S[a1->{ a2 a3  }]" -> Set(StandardMove("a1", "a2"), StandardMove("a1", "a3")),
      
      "p[a1->a3 R]" -> Set(PromotionMove("a1", "a2", "R"), PromotionMove("a1", "a3", "R")),
      "P[a1->{ a2 a3} Q]" -> Set(PromotionMove("a1", "a2", "Q"), PromotionMove("a1", "a3", "Q")),
      
      "c[ wq bk]" -> Set(CastleMove("wq"), CastleMove("bk")),
      "C[wk]" -> Set(CastleMove("wk")),
      
      "e[a1 a2]" -> Set(EnpassantMove("a1", "a2")),
      "E[d5 e6]" -> Set(EnpassantMove("d5", "e6"))
      )
      
  validParseCases.iterator foreach { pair =>
    val (encoded, expected) = pair
    s"$encoded" must s"parse to $expected" in {
      assert(expected == parseMoves(encoded).toSet)
    }
  }
}
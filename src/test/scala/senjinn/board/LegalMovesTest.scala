package senjinn.board

import org.scalatest._
import senjinn.parsers.MoveParsing
import senjinn.parsers.BoardParsing
import senjinn.parsers.FileLoadingTest
import senjinn.moves.Move
import senjinn.base.{loadResource}

/**
 * 
 */
class LegalMovesTest extends FlatSpec with FileLoadingTest with MoveParsing with BoardParsing
{
  executeAllTestCases()
  
  // FileLoadingTest API
  override type TestCaseArgs = (Board, Set[Move], Set[Move])
  
  override def resourceNameSequence: Seq[String] = {
    (1 until 11).iterator
      .map(n => s"legalmoves/case${("0" * (3 - n.toString.length))}${n.toString}")
      .toSeq
  }
  
  override def parseTestFile(lines: Seq[String]): TestCaseArgs = {
    val board = parseBoard(lines.take(9), 10)
    val expectedMoveLines = lines.drop(9).takeWhile(!_.startsWith("---"))
    val expectedAttackLines = lines.dropWhile(!_.startsWith("---")).drop(1)
    (board, parseMoves(expectedMoveLines).toSet, parseMoves(expectedAttackLines).toSet)
  }

  override def performTest(args: TestCaseArgs): Unit = {
    val (board, expectedMoves, expectedattacks) = args
    "Computed moves" must "match expected moves" in {
      val actualMoves = LegalMoves.computeMoves(board).toSet
      assert(expectedMoves == actualMoves, formatDifferences(expectedMoves, actualMoves))
    }
    "Computed attacks" must "match expected attacks" in {
      val actualAttacks = LegalMoves.computeAttacks(board).toSet
      assert(expectedattacks == actualAttacks, formatDifferences(expectedattacks, actualAttacks))
    }
  }

  private def formatDifferences(expected: Set[Move], actual: Set[Move]): String = {
    val missing = (expected -- actual).iterator.map(_.toString()).toSeq.sorted
    val extra = (actual -- expected).iterator.map(_.toString()).toSeq.sorted
    s"[$missing were expected but were missing. $extra were not expected but were present]"
  }
}
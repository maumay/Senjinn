package senjinn.board

import org.scalatest.FlatSpec
import senjinn.parsers.FileLoadingTest
import senjinn.parsers.BoardParsing
import senjinn.base.Square
import senjinn.parsers.ChessRegex

class PinnedPiecesTest extends FlatSpec with FileLoadingTest with BoardParsing
{
    executeAllTestCases()
  
    type TestCaseArgs = (String, Board, Set[Square])
  
    def resourceNameSequence: Seq[String] = Seq("pinnedpieces/case001", "pinnedpieces/case002")
    
    def parseTestFile(fileName: String, lines: Seq[String]): TestCaseArgs = {
      val (boardLines, pinnedLine) = (lines.take(9), s"${lines.last.trim.toUpperCase} ")
      val pinnedSquares = if (pinnedLine.matches("NONE ")) {
        Set[Square]()
      } else {
        val sqrx = ChessRegex.square
        require(pinnedLine.matches(s"(${sqrx.regex} +)+"), s"$fileName $pinnedLine")
        sqrx.findAllIn(pinnedLine).map(Square(_)).toSet
      }
      (fileName, parseBoard(boardLines, 20), pinnedSquares)
    }

    def performTest(args: TestCaseArgs): Unit = {
      val (fname, board, pinnedLocs) = args
      fname must "compute correct pinned pieces" in {
        val computed = PinnedPieces.compute(board).keySet
        assert(pinnedLocs == computed)
      }
    }
}
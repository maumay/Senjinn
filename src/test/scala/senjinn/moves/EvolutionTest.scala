package senjinn.moves

import org.scalatest.FlatSpec
import senjinn.parsers.MoveParsing
import senjinn.parsers.BoardParsing
import senjinn.base.{loadResource}
import senjinn.board.{ Board, HashCache, MoveReverser }
import senjinn.parsers.FileLoadingTest

/**
 */
class EvolutionTest extends FlatSpec with FileLoadingTest with MoveParsing with BoardParsing {
  
  executeAllTestCases()
  
  // FileLoadingTest API
  type TestCaseArgs = (Move, Board, Board)
  
  override def performTest(args: TestCaseArgs): Unit = {
    val (move, start, end) = args
    s"The move ${move.toCompactString}" must "evolve and devolve correctly" in {
      val startcpy = start.copy
      assertBoardstatesEqual(start, startcpy)
    	val reverser = new MoveReverser()
    	move.makeMove(start, reverser)
    	assertBoardstatesEqual(end, start)
    	move.undoMove(start, reverser)
    	assertBoardstatesEqual(startcpy, start)
    }
  }
  
  private def assertBoardstatesEqual(expected: Board, actual: Board) {
    val (e, a) = (expected, actual)
    assert(e.pieceLocations == a.pieceLocations)
    assert(e.hashCache == a.hashCache)
    assert(e.castleStatus == a.castleStatus)
    assert(e.piecesDeveloped == a.piecesDeveloped)
    assert(e.clock == a.clock)
    assert(e.enpassant == a.enpassant)
    assert(e.active == a.active)
  }
  
  override def testCaseIterator: Iterator[TestCaseArgs] = {
    (0 until 40).iterator
      .map(n => "case" + ("0" * (3 - n.toString.length)) + n.toString)
      .map(name => loadResource(testpkg, name))
      .map(parseTestFile(_))
  }

  private def parseTestFile(lines: Seq[String]): TestCaseArgs = {
    import HashCache.{ size => hcachesze }
    val initialMoveCount = 20
    val start = parseBoard(lines.slice(1, 10), initialMoveCount)
    val end = parseBoard(lines.slice(10, 19), initialMoveCount + 1)
    val nhcache = end.hashCache.copyCache
    nhcache((end.hashCache.currIndex - 1) % hcachesze) = start.computeHash
    val end2 = new Board(end.pieceLocations, HashCache(nhcache, end.hashCache.currIndex),
      end.castleStatus, end.piecesDeveloped, end.clock, end.enpassant, end.active)

    (parseMove(lines.head), start, end2)
  }

  private def parseMove(encoded: String): Move = {
    import senjinn.parsers.ChessRegex.{ castleZone => czregex }
    val lower = encoded.toLowerCase
    val (src, target) = "[a-h][1-8]".r.findAllIn(lower).toVector.splitAt(1)
    lower.head match {
      case 's' => StandardMove(src(0), target(0))
      case 'e' => EnpassantMove(src(0), target(0))
      case 'c' => CastleMove(czregex.findFirstIn(encoded).get)
      case 'p' => PromotionMove(src(0), target(0), "(?<=(result[=]))[nbrq]".r.findFirstIn(lower).get)
      case _   => throw new RuntimeException
    }
  }
}
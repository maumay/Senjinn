package parsers

import senjinn.moves.{ChessMove}

trait MoveParsing
{
  def parseStandardMoves(encodedMove: String): Vector[ChessMove] = {
    
    throw new RuntimeException
  }
}
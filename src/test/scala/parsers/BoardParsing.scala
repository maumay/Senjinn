package senjinn.parsers

import senjinn.board.{BoardState}

/**
 */
trait BoardParsing 
{
  final def parseBoard(attributes: Seq[String]): BoardState = {
    throw new RuntimeException
  }
}
package senjinn.parsers

import senjinn.parsers.ChessRegex._

object Main extends App 
{
  println(groupedSquares.regex)
  println("(f2 g2)".matches(groupedSquares.regex))
}
package senjinn.parsers

import senjinn.parsers.ChessRegex._

object Main extends App 
{
  //requirement failed: white_pieces: (f2 g2) (b3) (c4) (a1 h1) (c2) (e1)
  
  println(sixGroups.regex)
  println("white_pieces: (f2 g2) (b3) (c4) (a1 h1) (c2) (e1)".matches(whiteLocationsAttribute.regex))
}
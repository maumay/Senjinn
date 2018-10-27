package senjinn.base

import senjinn.base.ImplicitAreaConverters._
import scala.math.{abs}


/**
 * Represents a movement direction on a board in terms of the change 
 * in rank index and change in file index. The constructor is private, 
 * access instances through the companion object.
 */
class Dir private (val name: String, val deltaRank: Int, val deltaFile: Int) 
{
  override def toString = name
}

/**
 * Instantiates all possible instances of the Dir class, named values
 * are provided as well as a Vector of all possibilities.
 */
object Dir 
{
  private val c = new Dir(_, _, _)
  val (  n,   e,   s,   w) = (c("n", 1,  0), c("e", 0, -1), c("s", -1, 0),  c("w", 0, 1))
  val ( ne,  se,  sw,  nw) = (c("ne", 1, -1), c("se", -1, -1), c("sw", -1, 1),  c("nw", 1, 1))
  val (nne, nee, see, sse) = (c("nne", 2, -1), c("nee", 1, -2),  c("see", -1, -2), c("sse", -2, -1))
  val (ssw, sww, nww, nnw) = (c("ssw", -2, 1), c("sww", -1, 2),  c("nww", 1, 2),   c("nnw", 2, 1))
  
  val all = Vector(n, e, s, w, ne, se, sw, nw, nne, nee, see, sse, ssw, sww, nww, nnw)
}

/**
 * Represents one of the sides in a chess game. The constructor is 
 * private, access instances through the companion object.
 */
class Side private (val isWhite: Boolean) 
{
  val penultimatePawnRank = if (isWhite) 6 else 1
  
  def otherSide = if (isWhite) Side.black else Side.white
}

/**
 * Instantiates all possible instances of the Side class, named values
 * are provided as well as a Vector of all possibilities.
 */
object Side
{
  val (white, black) = (new Side(true), new Side(false))
  
  val all = Vector(white, black)
}


/**
 * Represents a zone into which it is possible to castle.
 */
class CastleZone private (val index: Int, val kingSrc: Square, val kingTarg: Square) 
{
  require(0 <= index && index < 4)
  val (isWhiteZone, isKingsideZone) = (kingSrc.rank == 0, kingTarg.file - kingSrc.file < 0)
  
  val rookSrc  = if (isKingsideZone) kingSrc  >> 3 else kingSrc  << 4
  val rookTarg = if (isKingsideZone) kingTarg << 1 else kingTarg >> 1
  
  /** The BoardSquareuares which must be free of enemy control before castling is legal. */
  val reqUncontrolledBoardSquares = isKingsideZone match {
    case true => (0 to 2).map(i => (kingSrc >> i): SquareSet).reduce(_|_)
    case _    => (0 to 2).map(i => (kingSrc << i): SquareSet).reduce(_|_)
  }
  
  /** The BoardSquareuares which must be free of all pieces before castling is legal. */
  val reqClearBoardSquares = isKingsideZone match {
    case true => (1 to 2).map(i => (kingSrc >> i): SquareSet).reduce(_|_)
    case _    => (1 to 3).map(i => (kingSrc << i): SquareSet).reduce(_|_)
  }
}

/**
 * Instantiates all possible instances of the CastleZone class, named values
 * are provided as well as a Vector of all possibilities.
 */
object CastleZone
{
  import Square._
  private val cons = new CastleZone(_, _, _)
  
  val whiteKingside  = cons(0, e1, g1)
  val whiteQueenside = cons(1, e1, c1)
  val blackKingside  = cons(2, e8, g8)
  val blackQueenside = cons(3, e8, c8)
  
  val all = Vector(whiteKingside, whiteQueenside, blackKingside, blackQueenside)
  val setOfNoZones = Set[CastleZone]()
  val setOfWkZone = Set(whiteKingside)
  val setOfWqZone = Set(whiteQueenside)
  val setOfWhiteZones = setOfWkZone ++ setOfWqZone
  val setOfBkZone = Set(blackKingside)
  val setOfBqZone = Set(blackQueenside)
  val setOfBlackZones = setOfBkZone ++ setOfBqZone
}


/**
 * Represents a piece which needs to be developed in the opening phase
 * of the game.
 */
class DevPiece private(val startSquare: Square)
{
}

object DevPiece
{
  private def cons = new DevPiece(_)
  
  val whiteKingsideKnight  = cons(Square.g1)
  val whiteKingsideBishop  = cons(Square.f1)
  val whiteKingsidePawn    = cons(Square.e2)
  val whiteQueensidePawn   = cons(Square.d2)
  val whiteQueensideBishop = cons(Square.c1)
  val whiteQueensideKnight = cons(Square.b1)
  
  val blackKingsideKnight  = cons(Square.g8)
  val blackKingsideBishop  = cons(Square.f8)
  val blackKingsidePawn    = cons(Square.e7)
  val blackQueensidePawn   = cons(Square.d7)
  val blackQueensideBishop = cons(Square.c8)
  val blackQueensideKnight = cons(Square.b8)
  
  val startSquareMap = Vector(
      whiteKingsideKnight,
      whiteKingsideBishop,
      whiteKingsidePawn,
      whiteQueensidePawn,
      whiteQueensideBishop,
      whiteQueensideKnight,
      blackKingsideKnight,
      blackKingsideBishop,
      blackKingsidePawn,
      blackQueensidePawn,
      blackQueensideBishop,
      blackQueensideKnight).map(p => (p.startSquare, p)).toMap
      
  def apply(startsquare: Square): DevPiece = startSquareMap(startsquare)
}

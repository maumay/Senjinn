package xawd.senjinn

import xawd.senjinn.ImplicitAreaConverters._
import scala.math.{abs}

// |----------------------------------------------------------------------------------------|

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
  
  val values = Vector(n, e, s, w, ne, se, sw, nw, nne, nee, see, sse, ssw, sww, nww, nnw)
}

// |----------------------------------------------------------------------------------------|

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
  
  val values = Vector(white, black)
}

// |----------------------------------------------------------------------------------------|

/**
 * Represents a zone into which it is possible to castle.
 */
class CastleZone private (val index: Int, val kingSrc: BoardSquare, val kingTarg: BoardSquare) 
{
  require(0 <= index && index < 4)
  val (isWhiteZone, isKingsideZone) = (kingSrc.rank == 0, kingTarg.file - kingSrc.file < 0)
  
  val rookSrc  = if (isKingsideZone) kingSrc  >> 3 else kingSrc  << 4
  val rookTarg = if (isKingsideZone) kingTarg << 1 else kingTarg >> 1
  
  /** The BoardSquareuares which must be free of enemy control before castling is legal. */
  val reqUncontrolledBoardSquares = isKingsideZone match {
    case true => (0 to 2).map(i => (kingSrc >> i): SquareSet).reduce(_ | _)
    case _    => (0 to 2).map(i => (kingSrc << i): SquareSet).reduce(_ | _)
  }
  
  /** The BoardSquareuares which must be free of all pieces before castling is legal. */
  val reqClearBoardSquares = isKingsideZone match {
    case true => (1 to 2).map(i => (kingSrc >> i): SquareSet).reduce(_ | _)
    case _    => (1 to 3).map(i => (kingSrc << i): SquareSet).reduce(_ | _)
  }
}

/**
 * Instantiates all possible instances of the CastleZone class, named values
 * are provided as well as a Vector of all possibilities.
 */
object CastleZone
{
  import BoardSquare._
  private val cons = new CastleZone(_, _, _)
  
  val whiteKingside  = cons(0, e1, g1)
  val whiteQueenside = cons(1, e1, c1)
  val blackKingside  = cons(2, e8, g8)
  val blackQueenside = cons(3, e8, c8)
  
  val values = Vector(whiteKingside, whiteQueenside, blackKingside, blackQueenside)
}

// |----------------------------------------------------------------------------------------|

/**
 * Represents a piece which needs to be developed in the opening phase
 * of the game.
 */
class DevelopmentPiece private(val startSquare: BoardSquare)
{
}

object DevelopmentPiece
{
  private def cons = new DevelopmentPiece(_)
  
  val whiteKingsideKnight  = cons(BoardSquare.g1)
  val whiteKingsideBishop  = cons(BoardSquare.f1)
  val whiteKingsidePawn    = cons(BoardSquare.e2)
  val whiteQueensidePawn   = cons(BoardSquare.d2)
  val whiteQueensideBishop = cons(BoardSquare.c1)
  val whiteQueensideKnight = cons(BoardSquare.b1)
  
  val blackKingsideKnight  = cons(BoardSquare.g8)
  val blackKingsideBishop  = cons(BoardSquare.f8)
  val blackKingsidePawn    = cons(BoardSquare.e7)
  val blackQueensidePawn   = cons(BoardSquare.d7)
  val blackQueensideBishop = cons(BoardSquare.c8)
  val blackQueensideKnight = cons(BoardSquare.b8)
  
  private val retrievalBySquareMap = Vector(
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
      
  def apply(startsquare: BoardSquare): DevelopmentPiece = retrievalBySquareMap(startsquare)
}

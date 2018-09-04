package xawd.senjinn

// ------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------
/**
 * An instance of the Direction class represents a movement direction on 
 * a board in terms of the change in rank index and change in file index.
 * The constructor is private, access instances through the companion 
 * object.
 */
class Direction private (val deltaRank: Int, val deltaFile: Int) 
{
}

/**
 * Instantiates all possible instances of the Direction class, named values
 * are provided as well as a Vector of all possibilities.
 */
object Direction 
{
  private val cons = new Direction(_, _)
  val (  n,   e,   s,   w) = (cons(1,  0), cons( 0, -1), cons(-1, 0),  cons(0, 1)  )
  val ( ne,  se,  sw,  nw) = (cons(1, -1), cons(-1, -1), cons(-1, 1),  cons(1, 1)  )
  val (nne, nee, see, sse) = (cons(2, -1), cons(1, -2),  cons(-1, -2), cons(-2, -1))
  val (ssw, sww, nww, nnw) = (cons(-2, 1), cons(-1, 2),  cons(1, 2),   cons(2, 1)  )
  
  val values = Vector(n, e, s, w, ne, se, sw, nw, nne, nee, see, sse, ssw, sww, nww, nnw)
}

// ------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------

/**
 * An instance of this class represents one of the sides in a chess game.
 * The constructor is private, access instances through the companion 
 * object.
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

// ------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------

import SquareSet.{ boardsquare2squareset }

/**
 * Represents a zone into which it is possible to castle.
 */
class CastleZone private (val kingSrc: Sq, val kingTarg: Sq) 
{
  val (isWhiteZone, isKingsideZone) = (kingSrc.rank == 0, kingTarg.file - kingSrc.file < 0)
  
  val rookSrc  = if (isKingsideZone) kingSrc  >> 3 else kingSrc  << 4
  val rookTarg = if (isKingsideZone) kingTarg << 1 else kingTarg >> 1
  
  /** The squares which must be free of enemy control before castling is legal. */
  val reqUncontrolledSquares = isKingsideZone match {
    case true => (0 to 2).map(i => (kingSrc >> i): SquareSet).reduce(_ | _)
    case _    => (0 to 2).map(i => (kingSrc << i): SquareSet).reduce(_ | _)
  }
  
  /** The squares which must be free of all pieces before castling is legal. */
  val reqClearSquares = isKingsideZone match {
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
  import Sq._
  private val cons = new CastleZone(_, _)
  
  val whiteKingside  = cons(e1, g1)
  val whiteQueenside = cons(e1, c1)
  val blackKingside  = cons(e8, g8)
  val blackQueenside = cons(e8, c8)
  
  val values = Vector(whiteKingside, whiteQueenside, blackKingside, blackQueenside)
}

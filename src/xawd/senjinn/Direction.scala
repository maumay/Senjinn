package xawd.senjinn


class Direction private (val deltaRank: Int, val deltaFile: Int) 
{
}

object Direction 
{
  private val cons = new Direction(_, _)
  val (  n,   e,   s,   w) = (cons(1,  0), cons( 0, -1), cons(-1, 0),  cons(0, 1)  )
  val ( ne,  se,  sw,  nw) = (cons(1, -1), cons(-1, -1), cons(-1, 1),  cons(1, 1)  )
  val (nne, nee, see, sse) = (cons(2, -1), cons(1, -2),  cons(-1, -2), cons(-2, -1))
  val (ssw, sww, nww, nnw) = (cons(-2, 1), cons(-1, 2),  cons(1, 2),   cons(2, 1)  )
  
  val values = Vector(n, e, s, w, ne, se, sw, nw, nne, nee, see, sse, ssw, sww, nww, nnw)
}


class Side private (val isWhite: Boolean) 
{
  val penultimatePawnRank = if (isWhite) 6 else 1
  
  def otherSide = if (isWhite) Side.black else this
}

object Side
{
  val (white, black) = (new Side(true), new Side(false))
}

class CastleZone private (val kingSrc: Sq, val kingTarg: Sq, val rookSrc: Sq, val rookTarg: Sq) 
{
  val (isWhiteZone, isKingsideZone) = (kingSrc.rank == 0, rookSrc.file == 0)
  
//  val reqUncontrolled
}

object CastleZone
{
  import Sq._
  private val cons = new CastleZone(_, _, _, _)
  
  val whiteKingside  = cons(e1, g1, h1, f1)
  val whiteQueenside = cons(e1, c1, a1, d1)
  val blackKingside  = cons(e8, g8, h8, f8)
  val blackQueenside = cons(e8, c8, a8, d8)
}
package xawd.senjinn

// |----------------------------------------------------------------------------------------|
/**
 * Represents one of the 64 squares on a chessboard, constructor is private and all 
 * possible values are enumerated in the companion object. 
 */
class BoardSquare private (val index: Int) 
{
  val (loc, rank, file) = (1L << index, index / 8, index % 8)
  
  def |(other: BoardSquare): SquareSet = {
    SquareSet(loc | other.loc)
  }
  
  def <<(shift: Int): BoardSquare = {
    BoardSquare.values(index + shift)
  }
  
  def >>(shift: Int): BoardSquare = {
    BoardSquare.values(index - shift)
  }
  
  def unary_~ = {
    SquareSet(~loc)
  }
  
  def nextSquare(dir: Direction): Option[BoardSquare] = {
    BoardSquare(rank + dir.deltaRank, file + dir.deltaFile)
  }
  
  def squaresLeft(dir: Direction): Int = nextSquare(dir) match {
    case None     => 0
    case Some(sq) => 1 + sq.squaresLeft(dir)
  }
  
  def allSquares(dirs: Iterable[Direction], proximity: Int = 8): Vector[BoardSquare] = {
    dirs.iterator.flatMap(dir => allSquares(dir, proximity)).toVector
  }
  
  def allSquares(dir: Direction, proximity: Int): Vector[BoardSquare] = proximity match {
    case 0 => Vector()
    case _ => nextSquare(dir) match {
        case None     => Vector()
        case Some(sq) => sq +: allSquares(dir, proximity - 1)
      }
  }
  
  override def toString(): String = {
    new String(Array(('h' - file).toChar, ('1' + rank).toChar))
  }
}

object BoardSquare 
{
  val values = (0 to 63).map(new BoardSquare(_)).toVector
  
  private val v = values
  val (h1, g1, f1, e1, d1, c1, b1, a1) = ( v(0),  v(1),  v(2),  v(3),  v(4),  v(5),  v(6),  v(7))
  val (h2, g2, f2, e2, d2, c2, b2, a2) = ( v(8),  v(9), v(10), v(11), v(12), v(13), v(14), v(15))
  val (h3, g3, f3, e3, d3, c3, b3, a3) = (v(16), v(17), v(18), v(19), v(20), v(21), v(22), v(23))
  val (h4, g4, f4, e4, d4, c4, b4, a4) = (v(24), v(25), v(26), v(27), v(28), v(29), v(30), v(31))
  val (h5, g5, f5, e5, d5, c5, b5, a5) = (v(32), v(33), v(34), v(35), v(36), v(37), v(38), v(39))
  val (h6, g6, f6, e6, d6, c6, b6, a6) = (v(40), v(41), v(42), v(43), v(44), v(45), v(46), v(47))
  val (h7, g7, f7, e7, d7, c7, b7, a7) = (v(48), v(49), v(50), v(51), v(52), v(53), v(54), v(55))
  val (h8, g8, f8, e8, d8, c8, b8, a8) = (v(56), v(57), v(58), v(59), v(60), v(61), v(62), v(63))
  
  
  def apply(name: String): BoardSquare = {
    val lower = name.toLowerCase.trim
    if (lower.matches("[a-h][1-8]")) { 
      v(('h' - lower(0)) + 8 * (lower(1) - '1')) 
    }
    else {
      throw new RuntimeException()
    }
  }
  
  def apply(rank: Int, file: Int): Option[BoardSquare] = {
    val inRange: Int => Boolean = x => -1 < x && x < 8
    if (inRange(rank) && inRange(file)) { 
      Some(BoardSquare.values(8 * rank + file)) 
    }
    else { None }
  }
  
  def apply(index: Int) = {
    values(index)
  }
  
  def unapply(square: BoardSquare) = Some((square.index, square.loc))
}

// |----------------------------------------------------------------------------------------|

/**
 * Wrapper for a primitive 64 bit integer which represents a collection of squares
 * on a chess board.
 */
class SquareSet private(val src: Long) extends AnyVal
{
  def |(other: SquareSet) = SquareSet(src | other.src)
  
  def &(other: SquareSet) = SquareSet(src & other.src)
  
  def ^(other: SquareSet) = SquareSet(src ^ other.src)
  
  def <<(shift: Int) = SquareSet(src << shift)
  
  def >>(shift: Int) = SquareSet(src >> shift)
  
  def unary_~ = SquareSet(~src)
  
  def intersects(square: BoardSquare): Boolean = {
    (square.loc & src) != 0
  }
  
  def squares: Iterator[BoardSquare] = (0 to 63).iterator
                                       .filter(i => ((1L << i) & src) != 0)
                                       .map(BoardSquare(_))
}

object SquareSet
{
  def apply() = new SquareSet(0L)
  
  def apply(arg: Long) = new SquareSet(arg)
  
  def apply(args: BoardSquare*) = new SquareSet(args.foldLeft(0L)(_ | _.loc))
  
}

// |----------------------------------------------------------------------------------------|

object ImplicitAreaConverters
{
  implicit def boardsquare2squareset(square: BoardSquare): SquareSet = SquareSet(square.loc)
  
  implicit def long2squareset(x: Long): SquareSet = SquareSet(x)
  
  implicit def squareset2long(s: SquareSet): Long = s.src

  implicit def boardsquare2long(square: BoardSquare): Long = square.loc
}


package senjinn.base

import enumeratum._

/**
 * Represents one of the 64 squares on a chess board.
 */
sealed abstract class Square(val index: Int) extends EnumEntry {

  /** A bitboard of cardinality 1 representing the location of this square. */
  val loc = 1L << index
  /** The index of the rank this square resides on */
  val rank = index / 8
  /** The index of the file this square resides on. */
  val file = index % 8

  /**
   * Performs a logical or operation on the location of this and that square.
   * @param other the other square to use in the binary or operation.
   */
  def |(other: Square): SquareSet = {
    SquareSet(loc | other.loc)
  }

  /**
   * Performs a left bit-shift operation on this square.
   * @param shift the number of places to shift this square to the left.
   * @return the square whose location is given by applying the given left shift to
   * this squares location.
   */
  def <<(shift: Int): Square = {
    Square(index + shift)
  }

  /**
   * Performs a right bit-shift operation on this square.
   * @param shift the number of places to shift this square to the right.
   * @return the square whose location is given by applying the given right shift to
   * this squares location.
   */
  def >>(shift: Int): Square = {
    Square(index - shift)
  }

  /**
   * Performs a logical 'not' operation on this square.
   * @return the bitboard representing all squares except this square.
   */
  def unary_~ = {
    SquareSet(~loc)
  }

  /**
   * Tests if this square intersects the given bitboard, i.e. if the bitboard
   * contains this square.
   * @param squares the set of squares to test membership of.
   */
  def intersects(squares: SquareSet): Boolean = {
    squares.intersects(this)
  }

  /**
   * Finds the next square in a given direction from this square.
   * @param dir the direction to search in.
   * @return the next square if it exists, nothing otherwise.
   */
  def nextSquare(dir: Dir): Option[Square] = {
    Square(rank + dir.deltaRank, file + dir.deltaFile)
  }

  /**
   * Computes how many squares are left in a given direction.
   * @param the direction to search in.
   */
  def squaresLeft(dir: Dir): Int = nextSquare(dir) match {
    case None     => 0
    case Some(sq) => 1 + sq.squaresLeft(dir)
  }

  /**
   * Finds all squares in all of the directions given which are within (inclusive) a
   * given distance (measured in number of steps away from this square).
   * @param dirs the directions to search in.
   * @param proximity the maximum number of steps away from this square that will be
   * searched.
   */
  def allSquares(dirs: Iterable[Dir], proximity: Int = 8): Vector[Square] = {
    dirs.iterator.flatMap(dir => allSquares(dir, proximity)).toVector
  }

  /**
   * Finds all squares in the given direction which are within (inclusive) a given 
   * distance (measured in number of steps away from this square).
   * @param dirs the direction to search in.
   * @param proximity the maximum number of steps away from this square that will be
   * searched.
   */
  def allSquares(dir: Dir, proximity: Int): Vector[Square] = proximity match {
    case 0 => Vector()
    case _ => nextSquare(dir) match {
      case None     => Vector()
      case Some(sq) => sq +: sq.allSquares(dir, proximity - 1)
    }
  }
}

/**
 * Companion object to `[[Square]]` which defines the possible values and some 
 * related functions.
 */
object Square extends Enum[Square] {

  /**
   * Recovers a square from its name, the input is case-insensitive.
   * @param name the name of the required square.
   * @return the matching square if the provided name is valid, nothing otherwise.
   */
  def apply(name: String): Option[Square] = {
    val lower = name.toLowerCase.trim
    if (lower.matches("[a-h][1-8]")) {
      Some(values(('h' - lower(0)) + 8 * (lower(1) - '1')))
    } else {
      None
    }
  }

  /**
   * Retrieves a square from its rank and file coordinates.
   * @param rank the index of the rank the required square resides on.
   * @param file the index of the file the required square resides on.
   * @return the matching square if the provided coordinates are valid, nothing
   * otherwise.
   */
  def apply(rank: Int, file: Int): Option[Square] = {
    val inRange: Int => Boolean = x => -1 < x && x < 8
    if (inRange(rank) && inRange(file)) {
      Some(values(8 * rank + file))
    } else { None }
  }

  /**
   * Retrieves a square from it's index. throws an exception if the provided index is
   * less than 0 or larger than 63.
   */
  def apply(index: Int): Square = {
    values(index)
  }

  /** Vector containing all the different instances of `[[Square]]`. */
  val values = findValues.toVector

  // First rank
  case object h1 extends Square(0)
  case object g1 extends Square(1)
  case object f1 extends Square(2)
  case object e1 extends Square(3)
  case object d1 extends Square(4)
  case object c1 extends Square(5)
  case object b1 extends Square(6)
  case object a1 extends Square(7)

  // Second rank
  case object h2 extends Square(8)
  case object g2 extends Square(9)
  case object f2 extends Square(10)
  case object e2 extends Square(11)
  case object d2 extends Square(12)
  case object c2 extends Square(13)
  case object b2 extends Square(14)
  case object a2 extends Square(15)

  // Third rank
  case object h3 extends Square(16)
  case object g3 extends Square(17)
  case object f3 extends Square(18)
  case object e3 extends Square(19)
  case object d3 extends Square(20)
  case object c3 extends Square(21)
  case object b3 extends Square(22)
  case object a3 extends Square(23)

  // Fourth rank
  case object h4 extends Square(24)
  case object g4 extends Square(25)
  case object f4 extends Square(26)
  case object e4 extends Square(27)
  case object d4 extends Square(28)
  case object c4 extends Square(29)
  case object b4 extends Square(30)
  case object a4 extends Square(31)

  // Fifth rank
  case object h5 extends Square(32)
  case object g5 extends Square(33)
  case object f5 extends Square(34)
  case object e5 extends Square(35)
  case object d5 extends Square(36)
  case object c5 extends Square(37)
  case object b5 extends Square(38)
  case object a5 extends Square(39)

  // Sixth rank
  case object h6 extends Square(40)
  case object g6 extends Square(41)
  case object f6 extends Square(42)
  case object e6 extends Square(43)
  case object d6 extends Square(44)
  case object c6 extends Square(45)
  case object b6 extends Square(46)
  case object a6 extends Square(47)

  // Seventh rank
  case object h7 extends Square(48)
  case object g7 extends Square(49)
  case object f7 extends Square(50)
  case object e7 extends Square(51)
  case object d7 extends Square(52)
  case object c7 extends Square(53)
  case object b7 extends Square(54)
  case object a7 extends Square(55)

  // Eighth rank
  case object h8 extends Square(56)
  case object g8 extends Square(57)
  case object f8 extends Square(58)
  case object e8 extends Square(59)
  case object d8 extends Square(60)
  case object c8 extends Square(61)
  case object b8 extends Square(62)
  case object a8 extends Square(63)
}

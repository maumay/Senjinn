package senjinn.base

import enumeratum._


sealed abstract class Square(val index: Int) extends EnumEntry {
  
  val (loc, rank, file) = (1L << index, index / 8, index % 8)

  def |(other: Square): SquareSet = {
    SquareSet(loc | other.loc)
  }

  def <<(shift: Int): Square = {
    Square(index + shift)
  }

  def >>(shift: Int): Square = {
    Square(index - shift)
  }

  def unary_~ = {
    SquareSet(~loc)
  }

  def intersects(squares: SquareSet): Boolean = {
    squares.intersects(this)
  }

  def nextSquare(dir: Dir): Option[Square] = {
    Square(rank + dir.deltaRank, file + dir.deltaFile)
  }

  def squaresLeft(dir: Dir): Int = nextSquare(dir) match {
    case None     => 0
    case Some(sq) => 1 + sq.squaresLeft(dir)
  }

  def allSquares(dirs: Iterable[Dir], proximity: Int = 8): Vector[Square] = {
    dirs.iterator.flatMap(dir => allSquares(dir, proximity)).toVector
  }

  def allSquares(dir: Dir, proximity: Int): Vector[Square] = proximity match {
    case 0 => Vector()
    case _ => nextSquare(dir) match {
      case None     => Vector()
      case Some(sq) => sq +: sq.allSquares(dir, proximity - 1)
    }
  }
}

object Square extends Enum[Square] {
  
  def apply(name: String): Square = {
    val lower = name.toLowerCase.trim
    if (lower.matches("[a-h][1-8]")) {
      values(('h' - lower(0)) + 8 * (lower(1) - '1'))
    } else {
      throw new RuntimeException()
    }
  }

  def apply(rank: Int, file: Int): Option[Square] = {
    val inRange: Int => Boolean = x => -1 < x && x < 8
    if (inRange(rank) && inRange(file)) {
      Some(values(8 * rank + file))
    } else { None }
  }

  def apply(index: Int): Square = {
    values(index)
  }

  //  def unapply(square: Square2) = {
  //    Some((square.index, square.loc))
  //  }

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

package senjinn.base

import senjinn.base.{ PieceMovementDirs => pmd }
import senjinn.base.BasicBitboards.{genEmptyBoardBitboards}
import enumeratum._


sealed trait Piece extends Moveable with EnumEntry {
  
  /** The set of 12 indices from each chesspiece must be equal to the range (0 until 12) */
  val index: Int
  /** The side this chesspiece belongs to */
  val side: Side
  /** A unique short string identifier for the piece */
  val shortName: String

  def isWhite = side.isWhite
  def isPawn  = (index % 6) == 0
  
  /**
   * Get the set of squares this piece can legally move to if it was the only piece on the 
   * board.
   */
  def getEmptyBoardMoveset(loc: Square): SquareSet
  
  /**
   * Get the set of squares this piece controls if it was the only piece on the board.
   */
  def getEmptyBoardControlset(loc: Square): SquareSet
}


object Piece extends Enum[Piece] {
  
  val pawns   = Vector(WhitePawn, BlackPawn)
  val knights = Vector(WhiteKnight, BlackKnight)
  val bishops = Vector(WhiteBishop, BlackBishop)
  val rooks   = Vector(WhiteRook, BlackRook)
  val queens  = Vector(WhiteQueen, BlackQueen)
  val kings   = Vector(WhiteKing, BlackKing)

  val values           = findValues.toVector
  val (whites, blacks) = (values.filter(_.isWhite), values.filterNot(_.isWhite))
  val nameMap          = values.map(p => (p.shortName, p)).toMap
  
  /** Retrieve a piece from it's index */
  def apply(index: Int): Piece = values(index)
  
  /** Retrieve a piece from its shorthand identifier */
  def apply(shortName: String): Piece = nameMap(shortName)
  
  /** Retrieve all pieces on a given side ordered by their index. */
  def apply(side: Side): Vector[Piece] = if (side.isWhite) whites else blacks

  
  // Piece implementation
  case object WhitePawn extends Piece {
    val index = 0
    val side = Side.White
    val shortName = "wp"

    def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      emptyBoardControl(loc.index)
    }

    def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      getControlset(loc, whites, blacks) & blacks
    }

    def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      val li = loc.index
      val empty = ~(whites | blacks)
      val push = (1L << (li + 8)) & empty
      val fpush = if (li < 16 && push != 0) push | ((1L << (li + 16)) & empty) else push
      fpush | getAttackset(loc, whites, blacks)
    }

    def getEmptyBoardMoveset(loc: Square) = {
      emptyBoardMoves(loc.index)
    }

    def getEmptyBoardControlset(loc: Square) = {
      emptyBoardControl(loc.index)
    }

    private val emptyBoardMoves: Array[Long] = {
      val arr = genEmptyBoardBitboards(pmd("wpm"), 1).toArray
      (8 to 15).foreach(i => arr(i) |= 1L << (i + 16))
      arr
    }

    private val emptyBoardControl: Array[Long] = genEmptyBoardBitboards(pmd("wpa"), 1)
  }

  case object WhiteKnight extends Piece {
    val index = 1
    val side = Side.White
    val shortName = "wn"

    def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      emptyBoardControl(loc.index)
    }

    def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      emptyBoardControl(loc.index) & blacks
    }

    def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      emptyBoardControl(loc.index) & ~whites
    }

    def getEmptyBoardMoveset(loc: Square) = {
      emptyBoardControl(loc.index)
    }

    def getEmptyBoardControlset(loc: Square) = {
      emptyBoardControl(loc.index)
    }

    private val emptyBoardControl: Array[Long] = genEmptyBoardBitboards(pmd("n"), 1)
  }

  case object WhiteBishop extends Piece {
    val index = 2
    val side = Side.White
    val shortName = "wb"

    def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      MagicBitboards.getBishControlset(loc, whites | blacks)
    }

    def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      getControlset(loc, whites, blacks) & blacks
    }

    def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      getControlset(loc, whites, blacks) & ~whites
    }

    def getEmptyBoardMoveset(loc: Square) = {
      emptyBoardControl(loc.index)
    }

    def getEmptyBoardControlset(loc: Square) = {
      emptyBoardControl(loc.index)
    }

    private val emptyBoardControl: Array[Long] = genEmptyBoardBitboards(pmd("b"))
  }

  case object WhiteRook extends Piece {
    val index = 3
    val side = Side.White
    val shortName = "wr"

    def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      MagicBitboards.getRookControlset(loc, whites | blacks)
    }

    def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      getControlset(loc, whites, blacks) & blacks
    }

    def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      getControlset(loc, whites, blacks) & ~whites
    }

    def getEmptyBoardMoveset(loc: Square) = {
      emptyBoardControl(loc.index)
    }

    def getEmptyBoardControlset(loc: Square) = {
      emptyBoardControl(loc.index)
    }

    private val emptyBoardControl: Array[Long] = genEmptyBoardBitboards(pmd("r"))
  }

  case object WhiteQueen extends Piece {
    val index = 4
    val side = Side.White
    val shortName = "wq"

    def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      WhiteBishop.getControlset(loc, whites, blacks) | WhiteRook.getControlset(loc, whites, blacks)
    }

    def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      getControlset(loc, whites, blacks) & blacks
    }

    def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      getControlset(loc, whites, blacks) & ~whites
    }

    def getEmptyBoardMoveset(loc: Square) = {
      WhiteBishop.getEmptyBoardMoveset(loc) | WhiteRook.getEmptyBoardMoveset(loc)
    }

    def getEmptyBoardControlset(loc: Square) = {
      getEmptyBoardMoveset(loc)
    }
  }

  case object WhiteKing extends Piece {
    val index = 5
    val side = Side.White
    val shortName = "wk"

    def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      emptyBoardControl(loc.index)
    }

    def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      emptyBoardControl(loc.index) & blacks
    }

    def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      emptyBoardControl(loc.index) & ~whites
    }

    def getEmptyBoardMoveset(loc: Square) = {
      emptyBoardControl(loc.index)
    }

    def getEmptyBoardControlset(loc: Square) = {
      emptyBoardControl(loc.index)
    }

    private val emptyBoardControl: Array[Long] = genEmptyBoardBitboards(pmd("k"), 1)
  }

  case object BlackPawn extends Piece {
    val index = 6
    val side = Side.Black
    val shortName = "bp"

    def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      emptyBoardControl(loc.index)
    }

    def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      getControlset(loc, whites, blacks) & whites
    }

    def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      val li = loc.index
      val empty = ~(whites | blacks)
      val push = (1L << (li - 8)) & empty
      val fpush = if (li > 47 && push != 0) push | ((1L << (li - 16)) & empty) else push
      fpush | getAttackset(loc, whites, blacks)
    }

    def getEmptyBoardMoveset(loc: Square) = {
      emptyBoardMoves(loc.index)
    }

    def getEmptyBoardControlset(loc: Square) = {
      emptyBoardControl(loc.index)
    }

    private val emptyBoardMoves: Array[Long] = {
      val arr = genEmptyBoardBitboards(pmd("bpm"), 1).toArray
      (48 to 55).foreach(i => arr(i) |= 1L << (i - 16))
      arr
    }

    private val emptyBoardControl: Array[Long] = genEmptyBoardBitboards(pmd("bpa"), 1)
  }

  case object BlackKnight extends Piece {
    val index = 7
    val side = Side.Black
    val shortName = "bn"

    def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      WhiteKnight.getControlset(loc, whites, blacks)
    }

    def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      getControlset(loc, whites, blacks) & whites
    }

    def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      getControlset(loc, whites, blacks) & ~blacks
    }

    def getEmptyBoardMoveset(loc: Square) = {
      WhiteKnight.getEmptyBoardMoveset(loc)
    }

    def getEmptyBoardControlset(loc: Square) = {
      WhiteKnight.getEmptyBoardControlset(loc)
    }
  }

  case object BlackBishop extends Piece {
    val index = 8
    val side = Side.Black
    val shortName = "bb"

    def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      MagicBitboards.getBishControlset(loc, whites | blacks)
    }

    def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      getControlset(loc, whites, blacks) & whites
    }

    def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      getControlset(loc, whites, blacks) & ~blacks
    }

    def getEmptyBoardMoveset(loc: Square) = {
      WhiteBishop.getEmptyBoardMoveset(loc)
    }

    def getEmptyBoardControlset(loc: Square) = {
      WhiteBishop.getEmptyBoardControlset(loc)
    }
  }

  case object BlackRook extends Piece {
    val index = 9
    val side = Side.Black
    val shortName = "br"

    def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      MagicBitboards.getRookControlset(loc, whites | blacks)
    }

    def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      getControlset(loc, whites, blacks) & whites
    }

    def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      getControlset(loc, whites, blacks) & ~blacks
    }

    def getEmptyBoardMoveset(loc: Square) = {
      WhiteRook.getEmptyBoardMoveset(loc)
    }

    def getEmptyBoardControlset(loc: Square) = {
      WhiteRook.getEmptyBoardControlset(loc)
    }
  }

  case object BlackQueen extends Piece {
    val index = 10
    val side = Side.Black
    val shortName = "bq"

    def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      BlackBishop.getControlset(loc, whites, blacks) | BlackRook.getControlset(loc, whites, blacks)
    }

    def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      getControlset(loc, whites, blacks) & whites
    }

    def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      getControlset(loc, whites, blacks) & ~blacks
    }

    def getEmptyBoardMoveset(loc: Square) = {
      BlackBishop.getEmptyBoardMoveset(loc) | BlackRook.getEmptyBoardMoveset(loc)
    }

    def getEmptyBoardControlset(loc: Square) = {
      getEmptyBoardMoveset(loc)
    }
  }

  case object BlackKing extends Piece {
    val index = 11
    val side = Side.Black
    val shortName = "bk"

    def getControlset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      WhiteKing.getControlset(loc, whites, blacks)
    }

    def getAttackset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      getControlset(loc, whites, blacks) & whites
    }

    def getMoveset(loc: Square, whites: SquareSet, blacks: SquareSet) = {
      getControlset(loc, whites, blacks) & ~blacks
    }

    def getEmptyBoardMoveset(loc: Square) = {
      WhiteKing.getEmptyBoardMoveset(loc)
    }

    def getEmptyBoardControlset(loc: Square) = {
      WhiteKing.getEmptyBoardControlset(loc)
    }
  }
}

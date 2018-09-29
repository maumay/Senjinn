package senjinn.pieces

import senjinn.base.{ PieceMovementDirs => pmd }
import senjinn.base.BasicBitboards.{genEmptyBoardBitboards}
import senjinn.base.ImplicitAreaConverters._
import senjinn.base.{BoardSquare, SquareSet, Side, MagicBitboards}

/**
 * Supertype of all chesspieces.  
 */
trait Moveable
{
  /** 
   *  Get the set of squares this piece is controlling given the locations of all pieces
   *  on the board.
   */
  def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet): SquareSet
  
  /** 
   *  Get the set of squares this piece can 'legally' move to which would result in a capture 
   *  of an enemy given the location of all pieces on the board. Note that this method is
   *  <b>not</b> expected to take into account illegal moves where the king is put into check.
   */
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet): SquareSet
  
  /** 
   *  Get the set of squares this piece can 'legally' move to given the location of all pieces.
   *  Note that this method is <b>not</b> expected to take into account illegal moves where 
   *  the king is put into check.
   */
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet): SquareSet
}


sealed trait ChessPiece extends Moveable
{
  /** The set of 12 indices from each chesspiece must be equal to the range (0 until 12) */
  val index: Int
  /** The side this chesspiece belongs to */
  val side: Side
  /** A unique short string identifier for the piece */
  val shortName: String

  def isWhite = side.isWhite
  
  /**
   * Get the set of squares this piece can legally move to if it was the only piece on the 
   * board.
   */
  def getEmptyBoardMoveset(loc: BoardSquare): SquareSet
  
  /**
   * Get the set of squares this piece controls if it was the only piece on the board.
   */
  def getEmptyBoardControlset(loc: BoardSquare): SquareSet
}


object ChessPiece
{
  val pawns   = Vector(WhitePawn, BlackPawn)
  val knights = Vector(WhiteKnight, BlackKnight)
  val bishops = Vector(WhiteBishop, BlackBishop)
  val rooks   = Vector(WhiteRook, BlackRook)
  val queens  = Vector(WhiteQueen, BlackQueen)
  val kings   = Vector(WhiteKing, BlackKing)

  val all = (pawns ++ knights ++ bishops ++ rooks ++ queens ++ kings).sortBy(_.index)
  val (whites, blacks) = (all.filter(_.isWhite), all.filterNot(_.isWhite))

  val nameMap = all.map(p => (p.shortName, p)).toMap
  
  /** Retrieve a piece from it's index */
  def apply(index: Int): ChessPiece = all(index)
  
  /** Retrieve a piece from its shorthand identifier */
  def apply(shortName: String): ChessPiece = nameMap(shortName)
  
  /** Retrieve all pieces on a given side ordered by their index. */
  def apply(side: Side): Vector[ChessPiece] = if (side.isWhite) whites else blacks
}


case object WhitePawn extends ChessPiece
{
  val index = 0
  val side = Side.white
  val shortName = "wp"
  
  def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    emptyBoardControl(loc.index)
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & blacks
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    val li = loc.index
    val empty = ~(whites | blacks)
    val push = (1L << (li + 8)) & empty
    val fpush = if (li < 16 && push != 0) push | ((1L << (li + 16)) & empty) else push
    fpush | getAttackset(loc, whites, blacks)
  }
  
  def getEmptyBoardMoveset(loc: BoardSquare) = {
    emptyBoardMoves(loc.index)
  }
  
  def getEmptyBoardControlset(loc: BoardSquare) = {
    emptyBoardControl(loc.index)
  }
  
  override def toString = "White Pawn"
  
  private val emptyBoardMoves: Array[Long] = {
    val arr = genEmptyBoardBitboards(pmd("wpm"), 1).toArray
    (8 to 15).foreach(i => arr(i) |= 1L << (i + 16))
    arr
  }
  
  private val emptyBoardControl: Array[Long] = genEmptyBoardBitboards(pmd("wpa"), 1)
}


case object WhiteKnight extends ChessPiece
{
  val index = 1
  val side = Side.white
  val shortName = "wn"
  
   def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    emptyBoardControl(loc.index)
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    emptyBoardControl(loc.index) & blacks
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    emptyBoardControl(loc.index) & ~whites
  }
  
  def getEmptyBoardMoveset(loc: BoardSquare) = {
    emptyBoardControl(loc.index)
  }
  
  def getEmptyBoardControlset(loc: BoardSquare) = {
    emptyBoardControl(loc.index)
  }
  
  override def toString = "White Knight"
  
  private val emptyBoardControl: Array[Long] = genEmptyBoardBitboards(pmd("n"), 1)
}


case object WhiteBishop extends ChessPiece
{
  val index = 2
  val side = Side.white
  val shortName = "wb"
  
   def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    MagicBitboards.getBishControlset(loc, whites | blacks)
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & blacks
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~whites
  }
  
  def getEmptyBoardMoveset(loc: BoardSquare) = {
    emptyBoardControl(loc.index)
  }
  
  def getEmptyBoardControlset(loc: BoardSquare) = {
    emptyBoardControl(loc.index)
  }
  
  override def toString = "White Bishop"
  
  private val emptyBoardControl: Array[Long] = genEmptyBoardBitboards(pmd("b"))
}


case object WhiteRook extends ChessPiece
{
  val index = 3
  val side = Side.white
  val shortName = "wr"
  
   def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    MagicBitboards.getRookControlset(loc, whites | blacks)
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & blacks
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~whites
  }
  
  def getEmptyBoardMoveset(loc: BoardSquare) = {
    emptyBoardControl(loc.index)
  }
  
  def getEmptyBoardControlset(loc: BoardSquare) = {
    emptyBoardControl(loc.index)
  }
  
  override def toString = "White Rook"
  
  private val emptyBoardControl: Array[Long] = genEmptyBoardBitboards(pmd("r"))
}


case object WhiteQueen extends ChessPiece
{
  val index = 4
  val side = Side.white
  val shortName = "wq"
  
   def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    WhiteBishop.getControlset(loc, whites, blacks) | WhiteRook.getControlset(loc, whites, blacks)
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & blacks
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~whites
  }
  
  def getEmptyBoardMoveset(loc: BoardSquare) = {
    WhiteBishop.getEmptyBoardMoveset(loc) | WhiteRook.getEmptyBoardMoveset(loc)
  }
  
  def getEmptyBoardControlset(loc: BoardSquare) = {
    getEmptyBoardMoveset(loc)
  }
  
  override def toString = "White Queen"
}


case object WhiteKing extends ChessPiece
{
  val index = 5
  val side = Side.white
  val shortName = "wk"
  
   def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    emptyBoardControl(loc.index)
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    emptyBoardControl(loc.index) & blacks
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    emptyBoardControl(loc.index) & ~whites
  }
  
  def getEmptyBoardMoveset(loc: BoardSquare) = {
    emptyBoardControl(loc.index)
  }
  
  def getEmptyBoardControlset(loc: BoardSquare) = {
    emptyBoardControl(loc.index)
  }
  
  override def toString = "White King"
  
  private val emptyBoardControl: Array[Long] = genEmptyBoardBitboards(pmd("k"), 1)
}


case object BlackPawn extends ChessPiece
{
  val index = 6
  val side = Side.black
  val shortName = "bp"
  
   def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    emptyBoardControl(loc.index)
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & whites
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    val li = loc.index
    val empty = ~(whites | blacks)
    val push = (1L << (li - 8)) & empty
    val fpush = if (li > 47 && push != 0) push | ((1L << (li - 16)) & empty) else push
    fpush | getAttackset(loc, whites, blacks)
  }
  
  def getEmptyBoardMoveset(loc: BoardSquare) = {
    emptyBoardMoves(loc.index)
  }
  
  def getEmptyBoardControlset(loc: BoardSquare) = {
    emptyBoardControl(loc.index)
  }
  
  override def toString = "Black Pawn"
  
  private val emptyBoardMoves: Array[Long] = {
    val arr = genEmptyBoardBitboards(pmd("bpm"), 1).toArray
    (48 to 55).foreach(i => arr(i) |= 1L << (i - 16))
    arr
  }
  
  private val emptyBoardControl: Array[Long] = genEmptyBoardBitboards(pmd("bpa"), 1)
}


case object BlackKnight extends ChessPiece
{
  val index = 7
  val side = Side.black
  val shortName = "bn"
  
   def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    WhiteKnight.getControlset(loc, whites, blacks)
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & whites
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~blacks
  }
  
  def getEmptyBoardMoveset(loc: BoardSquare) = {
    WhiteKnight.getEmptyBoardMoveset(loc)
  }
  
  def getEmptyBoardControlset(loc: BoardSquare) = {
    WhiteKnight.getEmptyBoardControlset(loc)
  }
  
  override def toString = "Black Knight"
}


case object BlackBishop extends ChessPiece
{
  val index = 8
  val side = Side.black
  val shortName = "bb"
  
   def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    MagicBitboards.getBishControlset(loc, whites | blacks)
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & whites
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~blacks
  }
  
  def getEmptyBoardMoveset(loc: BoardSquare) = {
    WhiteBishop.getEmptyBoardMoveset(loc)
  }
  
  def getEmptyBoardControlset(loc: BoardSquare) = {
    WhiteBishop.getEmptyBoardControlset(loc)
  }
  
  override def toString = "Black Bishop"
}


case object BlackRook extends ChessPiece
{
  val index = 9
  val side = Side.black
  val shortName = "br"
  
   def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    MagicBitboards.getRookControlset(loc, whites | blacks)
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & whites
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~blacks
  }
  
  def getEmptyBoardMoveset(loc: BoardSquare) = {
    WhiteRook.getEmptyBoardMoveset(loc)
  }
  
  def getEmptyBoardControlset(loc: BoardSquare) = {
    WhiteRook.getEmptyBoardControlset(loc)
  }
  
  override def toString = "Black Rook"
}


case object BlackQueen extends ChessPiece
{
  val index = 10
  val side = Side.black
  val shortName = "bq"
  
   def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    BlackBishop.getControlset(loc, whites, blacks) | BlackRook.getControlset(loc, whites, blacks)
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & whites
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~blacks
  }
  
  def getEmptyBoardMoveset(loc: BoardSquare) = {
    BlackBishop.getEmptyBoardMoveset(loc) | BlackRook.getEmptyBoardMoveset(loc)
  }
  
  def getEmptyBoardControlset(loc: BoardSquare) = {
    getEmptyBoardMoveset(loc)
  }
  
  override def toString = "Black Queen"
}


case object BlackKing extends ChessPiece
{
  val index = 11
  val side = Side.black
  val shortName = "bk"
  
   def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    WhiteKing.getControlset(loc, whites, blacks)
  }
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & whites
  }
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getControlset(loc, whites, blacks) & ~blacks
  }
  
  def getEmptyBoardMoveset(loc: BoardSquare) = {
    WhiteKing.getEmptyBoardMoveset(loc)
  }
  
  def getEmptyBoardControlset(loc: BoardSquare) = {
    WhiteKing.getEmptyBoardControlset(loc)
  }
  
  override def toString = "Black King"
}

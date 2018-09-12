package xawd.senjinn

import xawd.senjinn.{ PieceMovementDirs => pmd }
import xawd.senjinn.BasicBitboards.{genEmptyBoardBitboards}
import xawd.senjinn.SquareSet.{long2squareset}


sealed trait ChessPiece 
{
  val index: Int
  val side: Side
  
  def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet): SquareSet
  
  def getAttackset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet): SquareSet
  
  def getMoveset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet): SquareSet
  
  def getEmptyBoardMoveset(loc: BoardSquare): SquareSet
  
  def getEmptyBoardControlset(loc: BoardSquare): SquareSet
}

object ChessPiece
{
  val pieces = Vector(WhitePawn)
}

case object WhitePawn extends ChessPiece
{
  val index = 0
  val side = Side.white
  
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
    (8 to 15).foreach(i => arr(i) |= BoardSquare(i + 16).loc)
    arr
  }
  
  private val emptyBoardControl: Array[Long] = genEmptyBoardBitboards(pmd("wpa"), 1)
}


case object WhiteKnight extends ChessPiece
{
  val index = 1
  val side = Side.white
  
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
  
   def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    MagicBitboards.bishMagicMove(loc, whites | blacks)
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
  
   def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    MagicBitboards.rookMagicMove(loc, whites | blacks)
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
    val fpush = if (li < 16 && push != 0) push | ((1L << (li - 16)) & empty) else push
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
    (8 to 15).foreach(i => arr(i) |= BoardSquare(i + 16).loc)
    arr
  }
  
  private val emptyBoardControl: Array[Long] = genEmptyBoardBitboards(pmd("bpa"), 1)
}

case object BlackKnight extends ChessPiece
{
  val index = 7
  val side = Side.black
  
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
  
   def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    MagicBitboards.bishMagicMove(loc, whites | blacks)
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
  
   def getControlset(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    MagicBitboards.rookMagicMove(loc, whites | blacks)
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


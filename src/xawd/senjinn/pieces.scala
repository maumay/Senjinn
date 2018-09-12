package xawd.senjinn

import xawd.senjinn.{ PieceMovementDirs => pmd }
import xawd.senjinn.BasicBitboardApi.{genEmptyBoardBitboards}
import xawd.senjinn.SquareSet.{long2squareset}


sealed trait ChessPiece 
{
  val index: Int
  val side: Side
  
  def getAreaControlled(loc: BoardSquare, whites: SquareSet, blacks: SquareSet): SquareSet
  
  def getAttacks(loc: BoardSquare, whites: SquareSet, blacks: SquareSet): SquareSet
  
  def getMoves(loc: BoardSquare, whites: SquareSet, blacks: SquareSet): SquareSet
}

object ChessPiece
{
  val pieces = Vector(WhitePawn)
}

case object WhitePawn extends ChessPiece
{
  val index = 0
  val side = Side.white
  
  def getAreaControlled(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    emptyBoardControl(loc.index)
  }
  
  def getAttacks(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    getAreaControlled(loc, whites, blacks) & blacks
  }
  
  def getMoves(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    val li = loc.index
    val empty = ~(whites | blacks)
    val push = (1L << (li + 8)) & empty
    val fpush = if (li < 16 && push != 0) push | ((1L << (li + 16)) & empty) else push
    fpush | getAttacks(loc, whites, blacks)
  }
  
  override def toString = "White Pawn"
  
  private val emptyBoardMoves: Array[Long] = {
    val arr = genEmptyBoardBitboards(pmd("wpm"), 1).toArray
    (8 to 15).foreach(i => arr(i) |= BoardSquare(i + 16).loc)
    arr
  }
  
  private val emptyBoardControl: Array[Long] = genEmptyBoardBitboards(pmd("bpa"), 1)
}


case object WhiteKnight extends ChessPiece
{
  val index = 1
  val side = Side.white
  
   def getAreaControlled(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getAttacks(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getMoves(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  override def toString = ""
}


case object WhiteBishop extends ChessPiece
{
  val index = 2
  val side = Side.white
  
   def getAreaControlled(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getAttacks(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getMoves(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  override def toString = ""
}

case object WhiteRook extends ChessPiece
{
  val index = 3
  val side = Side.white
  
   def getAreaControlled(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getAttacks(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getMoves(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  override def toString = ""
}

case object WhiteQueen extends ChessPiece
{
  val index = 4
  val side = Side.white
  
   def getAreaControlled(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getAttacks(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getMoves(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  override def toString = ""
}


case object WhiteKing extends ChessPiece
{
  val index = 5
  val side = Side.white
  
   def getAreaControlled(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getAttacks(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getMoves(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  override def toString = ""
}

case object BlackPawn extends ChessPiece
{
  val index = 6
  val side = Side.black
  
   def getAreaControlled(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getAttacks(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getMoves(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  override def toString = ""
}

case object BlackKnight extends ChessPiece
{
  val index = 7
  val side = Side.black
  
   def getAreaControlled(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getAttacks(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getMoves(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  override def toString = ""
}

case object BlackBishop extends ChessPiece
{
  val index = 8
  val side = Side.black
  
   def getAreaControlled(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getAttacks(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getMoves(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  override def toString = ""
}

case object BlackRook extends ChessPiece
{
  val index = 9
  val side = Side.black
  
   def getAreaControlled(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getAttacks(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getMoves(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  override def toString = ""
}

case object BlackQueen extends ChessPiece
{
  val index = 10
  val side = Side.black
  
   def getAreaControlled(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getAttacks(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getMoves(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  override def toString = ""
}

case object BlackKing extends ChessPiece
{
  val index = 11
  val side = Side.black
  
   def getAreaControlled(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getAttacks(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  def getMoves(loc: BoardSquare, whites: SquareSet, blacks: SquareSet) = {
    throw new RuntimeException
  }
  
  override def toString = ""
}

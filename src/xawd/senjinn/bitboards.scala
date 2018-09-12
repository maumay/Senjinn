package xawd.senjinn


import xawd.senjinn.BoardSquare._
import xawd.senjinn.SquareSet.boardsquare2squareset
import xawd.senjinn.SquareSet.long2squareset
import xawd.senjinn.SquareSet.squareset2long


object BasicBitboardApi
{
  def rank(index: Int): SquareSet = BasicBitboardImpl.ranks(index)
  
  def file(index: Int): SquareSet = BasicBitboardImpl.files(index)
  
  def diag(index: Int): SquareSet = BasicBitboardImpl.diagonals(index)
  
  def adiag(index: Int): SquareSet = BasicBitboardImpl.antidiagonals(index)
  
  def genEmptyBoardBitboards(dirs: Iterable[Direction], proximity: Int = 8): Array[Long] = {
    BoardSquare.values.map(sq => foldSquares(sq.allSquares(dirs, proximity)).src).toArray
  }
}




/**
 * Encapsulates the basic areas on a chessboard. More specifically it contains:
 * <ul>
 * <li>The ranks and files</li>
 * <li>The north-east diagonals and north-west diagonals (anti-diagonals)</li>
 * <li>The possible moves on an empty board for each piece type</li>
 * <li>The squares controlled on an empty board for each piece type.</li>
 * </ul>
 */
private object BasicBitboardImpl
{
  val ranks: Array[Long] = {
    (0 to 7).map(i => (0 to 7).map(j => (1L << 8 * i) << j).reduce(_ | _)).toArray
  }
  
  val files: Array[Long] = {
    (0 to 7).map(i => (0 to 7).map(j => (1L << i) << 8 * j).reduce(_ | _)).toArray
  }
  
  val diagonals: Array[Long] = {
    (0 to 14)
    .map(i => if (i < 8) i else 8 *(i - 7) + 7)
    .map(BoardSquare(_))
    .map(sq => foldSquares(sq +: sq.allSquares(Array(Direction.ne))).src)
    .toArray
  }
  
  val antidiagonals: Array[Long] = {
    (0 to 14)
    .map(i => if (i < 8) 7 - i else 8 *(i - 7))
    .map(BoardSquare(_))
    .map(sq => foldSquares(sq +: sq.allSquares(Array(Direction.nw))).src)
    .toArray
  }
//  
//  /**
//   * The possible moves for each piece types (white and black pawns are 
//   * distinguished) and for each square on the board.
//   */
//  val emptyBoardMoves: Array[Array[Long]] = {
//    import xawd.senjinn.{ PieceMovementDirs => pmd}
//    Array(
//        genWhitePawnMoves,
//        genBlackPawnMoves,
//        genEmptyBoardBitboards(pmd("n"), 1),
//        genEmptyBoardBitboards(pmd("b")),
//        genEmptyBoardBitboards(pmd("r")),
//        genEmptyBoardBitboards(pmd("q")),
//        genEmptyBoardBitboards(pmd("k"), 1)
//        )
//  }
//  
//  /**
//   * The areas of control for each piece types (white and black pawns 
//   * are distinguished) and for each square on the board.
//   */
//  val emptyBoardControl: Array[Array[Long]] = {
//    import xawd.senjinn.{ PieceMovementDirs => pmd}
//    Array(
//        genEmptyBoardBitboards(pmd("wpa"), 1),
//        genEmptyBoardBitboards(pmd("bpa"), 1),
//        emptyBoardMoves(2),
//        emptyBoardMoves(3),
//        emptyBoardMoves(4),
//        emptyBoardMoves(5),
//        emptyBoardMoves(6)
//        )
//  }
//  
//  private def genWhitePawnMoves: Array[Long] = {
//    import xawd.senjinn.{ PieceMovementDirs => pmd}
//    val arr = genEmptyBoardBitboards(pmd("wpm"), 1).toArray
//    (8 to 15).foreach(i => arr(i) |= BoardSquare(i + 16).loc)
//    arr.toArray
//  }
//  
//  private def genBlackPawnMoves: Array[Long] = {
//    import xawd.senjinn.{ PieceMovementDirs => pmd}
//    val arr = genEmptyBoardBitboards(pmd("bpm"), 1).toArray
//    (48 to 56).foreach(i => arr(i) |= BoardSquare(i - 16).loc)
//    arr.toArray
//  }
}

object MagicBitboards
{
  // Api
  def rookMagicMove(pieces: SquareSet, square: BoardSquare): SquareSet = {
    val occupancyvariation = pieces & rookOccupancyMasks(square.index)
    val magicnumber = rookMagicNumbers(square.index)
    val magicshift = rookMagicBitshifts(square.index)
    rookMagicMoves(square.index)(((occupancyvariation * magicnumber) >> magicshift).toInt)
  }
  
  def bishMagicMove(pieces: SquareSet, square: BoardSquare): SquareSet = {
    val occupancyvariation = pieces & bishOccupancyMasks(square.index)
    val magicnumber = bishMagicNumbers(square.index)
    val magicshift = bishMagicBitshifts(square.index)
    bishMagicMoves(square.index)(((occupancyvariation * magicnumber) >> magicshift).toInt)
  }
  
  // Implementation  
  import xawd.senjinn.{ PieceMovementDirs => pmd }
  private type Arr       = Array[Long]
	private type SquareArr = Array[Arr]
  
  // Occupancy variations
  private def bishOccupancyVariations: SquareArr = BoardSquare.values.map(sq => genOccupancyVariations(sq, pmd("b"))).toArray
  private def rookOccupancyVariations: SquareArr = BoardSquare.values.map(sq => genOccupancyVariations(sq, pmd("r"))).toArray
  
  
  private def genOccupancyVariations(square: BoardSquare, dirs: Iterable[Direction]): Arr = {
    val relevantSquares = dirs.iterator
    .map(d => (d, square.squaresLeft(d) - 1))
    .flatMap(p => square.allSquares(p._1, Math.max(0, p._2)))
    .map(x => x: SquareSet)
    .toVector
    
    bitwiseOrPowerset(relevantSquares).map(_.src).toArray
  }
  
  private def bitwiseOrPowerset(input: Vector[SquareSet]): Vector[SquareSet] = input match {
      case head +: tail => {
        val recursed = bitwiseOrPowerset(tail)
        recursed ++ recursed.map(sqs => sqs | head)
      }
      case x => x
    }
    
  // Occupancy masks
  private val bishOccupancyMasks: Arr = bishOccupancyVariations.map(_.last)
  private val rookOccupancyMasks: Arr = rookOccupancyVariations.map(_.last)
  
  
  // Magic bitshifts
  private val bishMagicBitshifts: Array[Int] = bishOccupancyMasks.map(x => 64 - java.lang.Long.bitCount(x))
  private val rookMagicBitshifts: Array[Int] = rookOccupancyMasks.map(x => 64 - java.lang.Long.bitCount(x))
  
  
  // Magic numbers
  private val bishMagicNumbers: Arr = Array (
			0x8480100440302L, 0x200200a1010000L, 0x4010040441508100L, 0x491040080030021L,
			0x21104080208000L, 0x1032011000000L, 0x41c0128080000L, 0x2002020201040200L,
			0x120430040040L, 0x201040812084209L, 0x4220801002204L, 0x8044502000000L,
			0x10031040000102L, 0x51008040004L, 0x10080a0090041000L, 0x4060002208040400L,
			0x480a000420042410L, 0x20880801041882L, 0x8005408011012L, 0x800048e004000L,
			0x1001820080000L, 0x203410080821L, 0x4800800410881800L, 0x411400022021000L,
			0x4200040080100L, 0x3200008024401L, 0x2000480001020402L, 0x1408080000820500L,
			0x1060004008400L, 0x2200200200b000L, 0xc018004002020200L, 0x1020001084500L,
			0x208334000086808L, 0x4040404200120L, 0x404004840040400L, 0x600800010104L,
			0x40004100401100L, 0x80820080041000L, 0xc004010040020820L, 0x12006202010182L,
			0x400880840040800L, 0xc008404401108L, 0x8011080100c800L, 0x1024010400200L,
			0x5010124000201L, 0x10a0221000400209L, 0x118080080800400L, 0x4008202000040L,
			0x905108220200001L, 0x40482008a200008L, 0xa0084040880L, 0x2220084020a80000L,
			0x241002020004L, 0x2500048408820000L, 0x42020224050000L, 0x20010200810000L,
			0x2002082086000L, 0x40020104460200L, 0x20084018820L, 0x110000a2420200L,
			0x2200000010020200L, 0x220021a0200L, 0x402041006020400L, 0x20110102040840L
			)

	private val rookMagicNumbers: Arr = Array(
			0x10800480a0104000L, 0x40002000403000L, 0x80281000200080L, 0x800c0800500280L,
			0x8200200410081200L, 0x4a00080200040510L, 0x2180408002000100L, 0x180004100002080L,
			0x4000800080204000L, 0x802010814000L, 0x444801000822001L, 0x801000680080L,
			0x1120808004000800L, 0x42000408060010L, 0x80800200110080L, 0x800140800500L,
			0x208000804008L, 0x140240400a201000L, 0x200808010002000L, 0x1010008201000L,
			0x8808018018400L, 0x808004000200L, 0x110808002000500L, 0x20021004084L,
			0x1802080004000L, 0x200040100040L, 0x200080803000L, 0x100080080284L,
			0x28080080800400L, 0x800040080020080L, 0x80400100a01L, 0x202100090000804aL,
			0x401282800020L, 0x200040401000L, 0x4200080801000L, 0x40800800801000L,
			0x800800801400L, 0x800c00800200L, 0x8000500224000801L, 0x800840800100L,
			0x90824002208000L, 0x420600050004000L, 0x406001010010L, 0x20100008008080L,
			0x200040801010010L, 0x20004008080L, 0x9008600010004L, 0x100010080420004L,
			0x800040002004c0L, 0x400080210100L, 0x200200081100080L, 0x8000880080100080L,
			0x1080082040080L, 0x4068810400020080L, 0x20801100400L, 0x1000202418100L,
			0x408001102501L, 0x11008042002852L, 0x8800406001043009L, 0x1012000821100442L,
			0x1000442080011L, 0x1001000c00020801L, 0x400082104821004L, 0x2080010140208402L
			)
			
	// Magic move databases
	private val rookMagicMoves: SquareArr = genMagicMoveDatabase(rookOccupancyVariations, rookMagicNumbers, rookMagicBitshifts, pmd("r"))
	private val bishMagicMoves: SquareArr = genMagicMoveDatabase(bishOccupancyVariations, bishMagicNumbers, bishMagicBitshifts, pmd("b"))
			
	private type MagicMoveCons = (SquareArr, Arr, Array[Int], Iterable[Direction])
	
	private def genMagicMoveDatabase(c: MagicMoveCons): SquareArr = {
    val (allOccVars, magicNums, magicShifts, dirs) = c
    (for (i <- 0 to 63) yield {
      val (sq, occVars, mn, ms) = (BoardSquare(i), allOccVars(i), magicNums(i), magicShifts(i))
      val result = new Array[Long](occVars.length)
      occVars.map(ov => (ov, ((ov * mn) >>> ms).toInt)).foreach(p => result(p._2) = calcControlSet(sq, p._1, dirs))
      result
    }).toArray
  }
  
  private def calcControlSet(sq: BoardSquare, occupancyVariation: SquareSet, dirs: Iterable[Direction]): SquareSet = {
    val ov = occupancyVariation
    dirs.iterator
    .map(dir => sq.allSquares(dir, 8))
    .flatMap(sqs => sqs.span(!ov.intersects(_)) match {case (h, t) => h ++ t.take(1)})
    .map(x => x: SquareSet)
    .reduce(_ | _)
  }
}
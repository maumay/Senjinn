package xawd.senjinn.bitboards

import xawd.senjinn.SquareSet
import xawd.senjinn.BoardSquare
import xawd.senjinn.Direction

/**
 * 
 */
private object MagicBitboardImpl
{
  
  private def genOccupancyVariations(square: BoardSquare, dirs: Iterable[Direction]): Vector[SquareSet] = {
    import xawd.senjinn.SquareSet.{boardsquare2squareset}
    val relevantSquares = dirs.iterator
    .map(d => (d, square.squaresLeft(d) - 1))
    .flatMap(p => square.allSquares(p._1, Math.max(0, p._2)))
    .map(x => x: SquareSet)
    .toVector
    
    bitwiseOrPowerset(relevantSquares)
  }
  
  /*
   * Need to test this.
   */
  private def bitwiseOrPowerset(input: Vector[SquareSet]): Vector[SquareSet] = input match {
      case head +: tail => {
        val recursed = bitwiseOrPowerset(tail)
        recursed ++ recursed.map(sqs => sqs | head)
      }
      case x => x
    }
    
  
}
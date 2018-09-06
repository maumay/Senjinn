package xawd.senjinn.bitboards

import xawd.senjinn.SquareSet._
import xawd.senjinn.BoardSquare._

object BitboardInit 
{
  val ranks: Vector[SquareSet] = {
    (0 to 7).map(i => (0 to 7).map(j => (1L << 8 * i) << j).reduce(_ | _): SquareSet).toVector
  }
                                 
  val files: Vector[SquareSet] = {
    (0 to 7).map(i => (0 to 7).map(j => (1L << i) << 8 * j).reduce(_ | _): SquareSet).toVector
  }
  
//  val diagonals: Vector[SquareSet] 
}
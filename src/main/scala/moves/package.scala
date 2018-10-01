package senjinn

import senjinn.base.{BoardSquare, SquareSet}

package object moves
{
  def standardMove(src: BoardSquare, target: BoardSquare) = {
    standardCache(src)(target)
  }

  private type Sq2SMove = Map[BoardSquare, StandardMove]

  private val standardCache: Map[BoardSquare, Sq2SMove] = {
    import senjinn.pieces.{WhiteKnight => wk, WhiteQueen => wq}
    val f = (src: BoardSquare, sqs: SquareSet) => {
      sqs.squares.map(t => (t, StandardMove(src, t))).toMap
    }
    val allsq = BoardSquare.all
    val knightMoves = allsq.map(src => (src, f(src, wk.getEmptyBoardMoveset(src)))).toMap
    val queenMoves  = allsq.map(src => (src, f(src, wq.getEmptyBoardMoveset(src)))).toMap
    knightMoves ++ queenMoves
  }


}

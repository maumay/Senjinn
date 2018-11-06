package senjinn.base

import enumeratum._

sealed abstract class Dir2(val deltaRank: Int, val deltaFile: Int) extends EnumEntry
{
}

object Dir2 extends Enum[Dir2]
{
  /**
   * Finds the direction of the line connecting the start square to the end square
   * if it exists.
   */
  def ofLineConnecting(start: Square2, end: Square2): Option[Dir2] = {
    import Math.{min, max, abs}
    val (deltarank, deltafile) = (end.rank - start.rank, end.file - start.file)
    val maxAbsDelta = max(abs(deltarank), abs(deltafile))
    val minAbsDelta = min(abs(deltarank), abs(deltafile))
    val normaliser = if (minAbsDelta == 0) maxAbsDelta else minAbsDelta
    
    if (start == end || maxAbsDelta % normaliser != 0 || minAbsDelta % normaliser != 0) {
      None
    }
    else {
      val (ndeltarank, ndeltafile) = (deltarank / normaliser, deltafile / normaliser)
      values.find(dir => dir.deltaRank == ndeltarank && dir.deltaFile == ndeltafile)
    }
  }
  
  val values = findValues
  
  case object n extends Dir2(1, 0)
  case object e extends Dir2(0, -1)
  case object s extends Dir2(-1, 0)
  case object w extends Dir2(0, 1)
  
  case object ne extends Dir2(1, -1)
  case object se extends Dir2(-1, -1)
  case object sw extends Dir2(-1, 1)
  case object nw extends Dir2(1, 1)
  
  case object nne extends Dir2(2, 1)
  case object nee extends Dir2(1, -2)
  case object see extends Dir2(-1, -2)
  case object sse extends Dir2(-2, -1)
  case object ssw extends Dir2(-2, 1)
  case object sww extends Dir2(-1, 2)
  case object nww extends Dir2(1, 2)
  case object nnw extends Dir2(2, 1)
}
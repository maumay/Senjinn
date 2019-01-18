package senjinn.base

import enumeratum._

/**
 * Represents a direction in which some piece can move on a board.
 * @param deltaRank is the index change in rank when moving one step
 * in this direction.
 * @param deltaFile is the index change in file when moving one step
 * in this direction.
 */
sealed abstract class Dir(val deltaRank: Int, val deltaFile: Int) extends EnumEntry {
}

/**
 * Defines the instances of the sealed class `[[Dir]]` as well as providing some 
 * useful related definitions.
 */
object Dir extends Enum[Dir] {
  
  /**
   * Finds the direction of the line connecting the start square to the end square if
   * it exists.
   */
  def ofLineConnecting(start: Square, end: Square): Option[Dir] = {
    import Math.{ min, max, abs }
    val (deltarank, deltafile) = (end.rank - start.rank, end.file - start.file)
    val maxAbsDelta = max(abs(deltarank), abs(deltafile))
    val minAbsDelta = min(abs(deltarank), abs(deltafile))
    val normaliser = if (minAbsDelta == 0) maxAbsDelta else minAbsDelta

    val nDoesntDiv = (x: Int) => x % normaliser != 0
    if (start == end || nDoesntDiv(maxAbsDelta) || nDoesntDiv(minAbsDelta)) {
      None
    } else {
      val (ndeltarank, ndeltafile) = (deltarank / normaliser, deltafile / normaliser)
      values.find(dir => dir.deltaRank == ndeltarank && dir.deltaFile == ndeltafile)
    }
  }

  /** Vector containing all instances of `[[Dir]]`. */
  val values = findValues.toVector

  /** North */
  case object n extends Dir(1, 0)
  /** East */
  case object e extends Dir(0, -1)
  /** South */
  case object s extends Dir(-1, 0)
  /** West */
  case object w extends Dir(0, 1)

  /** North-east */
  case object ne extends Dir(1, -1)
  /** South-east */
  case object se extends Dir(-1, -1)
  /** South-west */
  case object sw extends Dir(-1, 1)
  /** North-west */
  case object nw extends Dir(1, 1)

  /** North-north-east */
  case object nne extends Dir(2, -1)
  /** North-east-east */
  case object nee extends Dir(1, -2)
  /** South-east-east */
  case object see extends Dir(-1, -2)
  /** South-south-east */
  case object sse extends Dir(-2, -1)
  /** South-south-west */
  case object ssw extends Dir(-2, 1)
  /** South-west-west */
  case object sww extends Dir(-1, 2)
  /** North-west-west */
  case object nww extends Dir(1, 2)
  /** North-north-west */
  case object nnw extends Dir(2, 1)
}
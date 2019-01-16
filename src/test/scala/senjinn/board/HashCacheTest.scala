package senjinn.board;

import org.scalatest.FlatSpec

/**
 * `Hello there!` [[scala.Option]] 
 */
class HashCacheTest extends FlatSpec
{
  "The cache" must "correctly override elements upon incrementing" in {
    val size = HashCache.size
    val cache = HashCache((0 until size).map(_.toLong).toArray, 0)
    for (i <- 0 until 2*size) {
      val expectedDiscard: Long = if (i < size - 1) i + 1 else 0
      assert(expectedDiscard == cache.increment(0L))
      val expectedCache = (0 until size).map(_.toLong).toArray
      (0 until i + 2).take(size).foreach(expectedCache(_) = 0)
      assert(cache == HashCache(expectedCache, i + 1))
    }
  }

  "The cache" must "correctly replace elements upon decrementing" in {
    val size = HashCache.size
    val cache = HashCache((0 until size).map(_ => -1L).toArray, size)
    val replacements = (1 to size + 1).toArray
    for (i <- 0 until replacements.length) {
      cache.decrement(replacements(i))
      val expectedCache = replacements(0).toLong +: (0 until size - 1).map(_ => -1L).toArray
      (1 to i).foreach(j => expectedCache(size - j) = replacements(j))
      assert(HashCache(expectedCache, size - (i + 1)) == cache)
    }
  }

  "The cache" must "correctly determine if there are three reps" in {
    Seq((Array(0L, 1, 2, 4, 1, 5, 5, 7, 9, 100, -23, 1), 12, true),
    (Array(0L, 1, 2, 4, 1, 5, 5, 7, 9, 100, -23, 1), 11, false),
    (Array(0L, 1, 2, 4, 1, 5, 5, 7, 9, 100, -23, 2), 20, false),
    (Array(2L, 1, 2, 4, 1, 5, 5, 7, 9, 100, -23, 2), 20, true))
    .foreach(trip => {
      val (cache, mcount, hasThreeReps) = trip
      assert(hasThreeReps == HashCache(cache, mcount).hasthreeRepetitions)
    })
  }
}

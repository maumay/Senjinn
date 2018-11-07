package senjinn.board

/**
 * Fixed size cache which tracks the hashed values of states of play
 * over time.
 */
class HashCache private(private val cache: Array[Long], private var moveCount: Int)
{
  private var cacheIndex: Int = moveCount % HashCache.size

  def increment(newPosition: Long): Long = {
    moveCount += 1
    updateIndexer()
    val discard = cache(cacheIndex)
    cache(cacheIndex) = newPosition
    discard
  }

  def decrement(oldPosition: Long) {
    cache(cacheIndex) = oldPosition
    moveCount -= 1
    updateIndexer()
  }

  def hasthreeRepetitions = {
    if (moveCount < HashCache.size) false else {
      val cpy = cache.sorted
      var samecount = 1
      var last = cpy.head
      var index = 1
      while (index < HashCache.size && samecount < 3) {
        val next = cpy(index)
        if (next == last) samecount += 1 else {samecount = 1; last = next}
        index += 1
      }
      samecount == 3
    }
  }

  private def updateIndexer() { cacheIndex = moveCount % HashCache.size }

  def currIndex = cacheIndex
  def copy = new HashCache(cache.clone(), moveCount)
  def copyCache = cache.clone()
  
  // Object API
  override def toString(): String = {
    import java.util.Arrays
    s"HashCache[cache=${Arrays.toString(cache)}, index=$cacheIndex]"
  }

  override def equals(x: Any) = {
    x.isInstanceOf[HashCache] && {
      val other = x.asInstanceOf[HashCache]
      cache.toList == other.cache.toList
    }
  }

  override def hashCode = cache.toList.##
}

object HashCache
{
  val size = 12

  def apply(): HashCache = {
    apply(new Array[Long](size), 0)
  }

  def apply(cache: Array[Long], moveCount: Int): HashCache = {
    require(cache.length == size)
    new HashCache(cache.clone, moveCount)
  }
}

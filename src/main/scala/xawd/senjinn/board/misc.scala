
class HalfMoveCounter(private var _count: Int)
{
    def count = _count
    def increment { _count += 1 }
    def reset { _count = 0 }
    def copy = new HalfMoveCounter(_count)
}

class HashCache private(private val cache: Array[Long], private var moveCount: Int)
{
    private var indexer: Int = moveCount % HashCache.size 

    def increment(newPosition: Long): Long = { 
        moveCount += 1
        updateIndexer()
        val discard = cache(indexer)
        cache(indexer) = newPosition
        discard
    }

    def decrement(oldPosition: Long): Unit = {
        cache(indexer) = oldPosition
        moveCount -= 1
        updateIndexer()
    }

    def hasthreeRepetitions = {
        if (moveCount < HashCache.size) false else {
            val cpy = cache.sorted
            var samecount = 1
            var last = cache.head
            var index = 1
            while (index < HashCache.size && samecount < 3) {
                index += 1
                val next = cpy(index)
                if (next == last) samecount += 1 else {samecount = 0; last = next}
            }
            samecount == 3
        }
    }

    def copy = new HashCache(cache.clone(), moveCount)

    private def updateIndexer() { indexer = moveCount % HashCache.size }
}

object HashCache
{
    import xawd.senjinn.SquareSet

    val size = 12

    def apply(cache: Array[Long], moveCount: Int): HashCache = {
        require(cache.length == size)
        new HashCache(cache.clone, moveCount)
    }
}
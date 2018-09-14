
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

    

    def copy = new HashCache(cache.clone(), moveCount)

    private def updateIndexer() { indexer = moveCount % HashCache.size }
}

object HashCache
{
    val size = 12

}
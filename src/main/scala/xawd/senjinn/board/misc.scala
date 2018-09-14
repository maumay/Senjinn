
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

    def apply(): HashCache = {
        apply(new Array[Long](size), 0)
    }

    def apply(cache: Array[Long], moveCount: Int): HashCache = {
        require(cache.length == size)
        new HashCache(cache.clone, moveCount)
    }
}

import xawd.senjinn.CastleZone
import xawd.senjinn.Side
import scala.collection.{mutable => mutable}

class CastlingTracker private(private val _rights: mutable.Set[CastleZone], 
    private var _white: Option[CastleZone], private var _black: Option[CastleZone])
{
    def rights = _rights
    def white = _white
    def black = _black
    def status(side: Side) = if (side.isWhite) _white else _black
    // def 

    def remove(sideStatus: CastleZone) {
        if (sideStatus.isWhiteZone) {
            assert(_white.get == sideStatus)
            _white = None
        }
        else {
            assert(_black.get == sideStatus)
            _black = None
        }
    }
}

object CastlingTracker
{
    private type TrackerCons = (Iterable[CastleZone], Option[CastleZone], Option[CastleZone])

    def apply(cons: TrackerCons) = {
        val (rights, white, black) = cons
        new CastlingTracker(mutable.HashSet(rights.iterator.toSeq: _*), white, black)
    }

    def apply(): CastlingTracker = {
        apply(mutable.HashSet(CastleZone.values: _*), None, None)
    }
}
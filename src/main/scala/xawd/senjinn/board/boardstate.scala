package xawd.senjinn.board

import scala.collection.{mutable => mutable}
import xawd.senjinn.{DevelopmentPiece, Side, BoardSquare, ChessPiece, CastleZone}

/**
 * A mutable representation of the state of a chessboard, it
 * is designed so that a move can mutate it in the forward 
 * direction and reverse it using an appropriate instance of
 * {@link MoveReverser}.  
 */
 class BoardState(
 	val plocs: PieceLocations,
    val hcache: HashCache,
	val cstatus: CastlingTracker,
	val pdev: mutable.Set[DevelopmentPiece],
	val clock: HalfMoveCounter,
	private var _active: Side,
	private var _enpassant: Option[BoardSquare]
	)
 {
 	def active = _active
 	def switchActive() {_active = _active.otherSide }
 	def computeHash = plocs.hash ^ BoardHasher.hashFeatures(active, enpassant, cstatus)
 	def enpassant = _enpassant
 	def enpassantAvailable = enpassant.isDefined
 	def clearEnpassant() {_enpassant = None}
 	def enpassant_=(square: BoardSquare) {_enpassant = Some(square)}
 }

object BoardState 
{

}

/**
 * Keeps track of the state of the half move count in a game,
 * it is required to implement the 50 move rule. 
 */
class HalfMoveCounter(private var _count: Int)
{
    def count = _count
    def count_=(x: Int) {_count = x }
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



class CastlingTracker private(private val _rights: mutable.Set[CastleZone], 
    private var _white: Option[CastleZone], private var _black: Option[CastleZone])
{
    def rights = _rights
    def white = _white
    def black = _black
    def status(side: Side) = if (side.isWhite) _white else _black

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

    def copy = CastlingTracker(_rights, _white, _black)
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



class MoveReverser
{
    var consumed = true
    var discardedCastleRights: Set[CastleZone] = Set()
    var pieceTaken: Option[ChessPiece] = None
    var pieceDeveloped: Option[DevelopmentPiece] = None
    var discardedEnpassant: Option[BoardSquare] = None
    var discardedHash: Long = 0L
    var discardedHalfmoveClock: Int = -1
}

object MoveReverser
{
    def apply() = new MoveReverser()
}

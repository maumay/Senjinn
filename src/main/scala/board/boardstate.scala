package senjinn.board

import scala.collection.{mutable => mutable}
import senjinn.base.{DevelopmentPiece, Side, BoardSquare, CastleZone, SquareSet}
import senjinn.pieces.ChessPiece


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
  var enpassant: Option[BoardSquare],
  private var _active: Side)
{
  def active = _active
  def switchActive() {_active = _active.otherSide }
  def computeHash = plocs.hash ^ BoardHasher.hashFeatures(active, enpassant, cstatus)
}

object BoardState 
{
}


/**
 * Keeps track of the state of the half move count in a game,
 * it is required to implement the 50 move rule. 
 */
class HalfMoveCounter(var count: Int)
{
  def increment { count += 1 }
  def reset { count = 0 }
  def copy = new HalfMoveCounter(count)
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

  def decrement(oldPosition: Long) {
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
  val size = 12

  def apply(): HashCache = {
    apply(new Array[Long](size), 0)
  }

  def apply(cache: Array[Long], moveCount: Int): HashCache = {
    require(cache.length == size)
    new HashCache(cache.clone, moveCount)
  }
}


class CastlingTracker private(val rights: mutable.Set[CastleZone], 
    private var _white: Option[CastleZone], private var _black: Option[CastleZone])
{
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

  def copy = CastlingTracker(rights, _white, _black)
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
  var discardedCastleRights: Set[CastleZone] = Set()
  var isConsumed = true
  var pieceTaken: Option[ChessPiece] = None
  var pieceDeveloped: Option[DevelopmentPiece] = None
  var discardedEnpassant: Option[BoardSquare] = None
  var discardedHash: Long = 0L
  var discardedClockValue: Int = -1

  def clearCastleRights() {
    discardedCastleRights = MoveReverser.emptyRights
  }
}

object MoveReverser
{
  val emptyRights = Set[CastleZone]()
  def apply() = new MoveReverser()
}

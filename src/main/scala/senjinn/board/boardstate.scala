package senjinn.board

import scala.collection.{mutable => mutable}
import senjinn.base.{DevPiece, Side, Square, CastleZone, SquareSet}
import senjinn.base.pieces.ChessPiece


/**
  * A mutable representation of the state of a chessboard, it
  * is designed so that a move can mutate it in the forward 
  * direction and reverse it using an appropriate instance of
  * {@link MoveReverser}.  
  */
class BoardState(
  val pieceLocations: PieceLocations,
  val hashCache: HashCache,
  val castleStatus: CastlingTracker,
  val piecesDeveloped: mutable.Set[DevPiece],
  var clock: Int,
  var enpassant: Option[Square],
  private var _active: Side)
{
  def active = _active
  
  def passive = Side.other(active)
  
  def switchActive() {_active = Side.other(_active) }
  
  def computeHash = {
    pieceLocations.hash ^ BoardHasher.hashFeatures(active, enpassant, castleStatus)
  }
  
  def copy: BoardState = new BoardState(pieceLocations.copy,
		  hashCache.copy, castleStatus.copy, piecesDeveloped.to[mutable.Set],
		  clock, enpassant, _active)
  

  override def equals(x: Any) = {
    x.isInstanceOf[BoardState] && {
      val o = x.asInstanceOf[BoardState]
      fieldTuple == o.fieldTuple
    }
  }
  
  override def hashCode(): Int = fieldTuple.##
  
  private def fieldTuple = (pieceLocations, hashCache, 
      castleStatus, piecesDeveloped, clock, enpassant, active)
}

object BoardState
{
    def apply(properties: Map[String, Any]): BoardState = {
      val p = properties
      new BoardState(
          p("pieceLocations").asInstanceOf[PieceLocations],
          p("hashCache").asInstanceOf[HashCache],
          p("castleStatus").asInstanceOf[CastlingTracker],
          p("piecesDeveloped").asInstanceOf[mutable.Set[DevPiece]],
          p("clock").asInstanceOf[Int],
          p("enpassant").asInstanceOf[Option[Square]],
          p("active").asInstanceOf[Side]
      )
    }
}

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
      var last = cache.head
      var index = 1
      while (index < HashCache.size && samecount < 3) {
        val next = cpy(index)
        if (next == last) samecount += 1 else {samecount = 0; last = next}
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


class CastlingTracker private(val rights: mutable.Set[CastleZone],
  private var _white: Option[CastleZone], private var _black: Option[CastleZone])
{
  def white = _white
  def black = _black
  def status(side: Side) = if (side.isWhite) _white else _black

  def setStatus(sideStatus: CastleZone) {
    if (sideStatus.isWhiteZone){
      assert(_white.isEmpty)
      _white = Some(sideStatus)
    }
    else {
      assert(_black.isEmpty)
      _black = Some(sideStatus)
    }
  }

  def removeStatus(sideStatus: CastleZone) {
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

  override def equals(x: Any) = {
    x.isInstanceOf[CastlingTracker] && {
      val o = x.asInstanceOf[CastlingTracker]
        (rights, white, black) == (o.rights, o.white, o.black)
    }
  }

  override def hashCode = (rights, white, black).##
}

object CastlingTracker
{
  private type TrackerCons = (Iterable[CastleZone], Option[CastleZone], Option[CastleZone])

  def apply(cons: TrackerCons) = {
    val (rights, white, black) = cons
    new CastlingTracker(mutable.HashSet(rights.toSeq: _*), white, black)
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
  var pieceDeveloped: Option[DevPiece] = None
  var discardedEnpassant: Option[Square] = None
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

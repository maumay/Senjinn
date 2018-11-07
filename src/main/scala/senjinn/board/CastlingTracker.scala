package senjinn.board

import scala.collection.{mutable => mutable}
import senjinn.base.{CastleZone, Side}

/**
 * Tracks information relating to the status of castling for 
 * both sides
 */
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
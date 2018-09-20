package xawd.senjinn.board

import scala.collection.{mutable => mutable}
import xawd.senjinn.{DevelopmentPiece, Side, BoardSquare}

/**
 *
 */
 class BoardState(
 	private val plocs: PieceLocations,
    private val hcache: HashCache,
	private val cstatus: CastlingTracker,
	private val pdev: mutable.Set[DevelopmentPiece],
	private var active: Side,
	private var enpassant: Option[BoardSquare]
	)
 {

 }

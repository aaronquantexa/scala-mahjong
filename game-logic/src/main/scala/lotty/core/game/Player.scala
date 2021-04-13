package lotty.core.game

import lotty.core.model.{Tile, Util}

final case class Player(
                         playerId: PlayerId,
                         displayedTiles: Seq[Tile],
                         private[core] val handTile: Seq[Tile]
                       ) {

  def chow(tile1ToChowWith: Tile,
           tile2ToChowWith: Tile,
           actionableTileToChow: Tile,
           tileToDiscard: Tile): Player = {

    this.copy(
      displayedTiles = this.displayedTiles ++ Seq(tile1ToChowWith, tile2ToChowWith, actionableTileToChow),
      handTile = subtract(this.handTile, Seq(tile1ToChowWith, tile2ToChowWith, tileToDiscard))
    )
  }

  def pung(tileToPung: Tile,
           actionableTileToPung: Tile,
           tileToDiscard: Tile): Player = {
    this.copy(
      displayedTiles = this.displayedTiles ++ Seq.fill(3)(tileToPung),
      handTile = subtract(this.handTile, (Seq(tileToPung, tileToPung, tileToDiscard)))
    )
  }

  /**
   * Subtraction operator
   *
   * @param leftHandSide Tiles
   * @param rightHandSide Tiles
   * @return leftHandSide - rightHandSide
   */
  def subtract(leftHandSide: Seq[Tile], rightHandSide: Seq[Tile]): Seq[Tile] = {
    rightHandSide.foldLeft(leftHandSide) {
      case (tilesSeq, tileToMove) =>
        val indexOfTile = tilesSeq.indexOf(tileToMove)
        if (indexOfTile >= 0) {
          tilesSeq.take(indexOfTile) ++ tilesSeq.drop(indexOfTile + 1)
        }
        else {
          tilesSeq
        }
    }
  }

  def showPretty (): Unit = {
    println ("PLAYER " + playerId.name)
    println (" -- Displayed tiles -- ")
    println (displayedTiles.show () )
    println (" -- Hand -- ")
    println (handTile.sortBy (Util.circlesFirstSort).show () )
  }

}

final case class PlayerId(name: String)
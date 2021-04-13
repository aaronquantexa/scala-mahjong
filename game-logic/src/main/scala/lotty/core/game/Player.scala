package lotty.core.game

import lotty.core.model.Tile

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
      handTile = this.handTile subtract Seq(tile1ToChowWith, tile2ToChowWith, tileToDiscard)
    )
  }

  def pung(tileToPung: Tile,
           actionableTileToPung: Tile,
           tileToDiscard: Tile): Player = {
    this.copy(
      displayedTiles = this.displayedTiles ++ Seq.fill(3)(tileToPung),
      handTile = this.handTile subtract Seq(tileToPung, tileToPung, tileToDiscard)
    )
  }
}

final case class PlayerId(name: String)
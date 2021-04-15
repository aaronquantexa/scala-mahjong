package lotty.core.game

import lotty.core.model.{Chow, Flower, Kong, Meld, Pung, Tile}

final case class Player(
                         playerId: PlayerId,
                         displayedFlowers: Seq[Flower],
                         displayedTiles: Seq[Meld],
                         private[core] val handTile: Seq[Tile]
                       ) {

  def chow(tile1ToChowWith: Tile,
           tile2ToChowWith: Tile,
           actionableTileToChow: Tile,
           tileToDiscard: Tile): Player = {

    this.copy(
      displayedTiles = this.displayedTiles ++ Seq(Chow(tile1ToChowWith, tile2ToChowWith, actionableTileToChow)),
      handTile = this.handTile subtract Seq(tile1ToChowWith, tile2ToChowWith, tileToDiscard)
    )
  }

  def pung(tileToPung: Tile,
           actionableTileToPung: Tile,
           tileToDiscard: Tile): Player = {
    this.copy(
      displayedTiles = this.displayedTiles ++ Seq(Pung(tileToPung)),
      handTile = this.handTile subtract Seq(tileToPung, tileToPung, tileToDiscard)
    )
  }

  def kong(tileToKong: Tile,
           actionableTileToKong: Tile,
           tilePicked: Tile,
           tileToDiscard: Tile): Player = {
    this.copy(
      displayedTiles = this.displayedTiles ++ Seq(Kong(tileToKong)),
      handTile = (this.handTile ++ Seq(tilePicked)) subtract Seq(tileToKong, tileToKong, tileToKong, tileToDiscard)
    )
  }
}

final case class PlayerId(name: String)
package lotty.core.game

import lotty.core.model.{Chow, Flower, Kong, Meld, Pung, Tile}

final case class Player(
                         playerId: PlayerId,
                         displayedFlowers: Seq[Flower],
                         displayedTiles: Seq[Meld],
                         private[core] val handTile: Seq[Tile]
                       )

object Player {

  def chow(tile1ToChowWith: Tile, tile2ToChowWith:Tile, actionableTileToChow: Tile, tileToDiscard: Tile): Player => Player = {
    (player: Player) =>
      player.copy(
        displayedTiles = player.displayedTiles ++ Seq(Chow(tile1ToChowWith, tile2ToChowWith, actionableTileToChow)),
        handTile = player.handTile subtract Seq(tile1ToChowWith, tile2ToChowWith, tileToDiscard)
      )
  }

  def pung(tileToPung: Tile, tileToDiscard:Tile): Player => Player = {
    (player: Player) =>
      player.copy(
        displayedTiles = player.displayedTiles ++ Seq(Pung(tileToPung)),
        handTile = player.handTile subtract Seq(tileToPung, tileToPung, tileToDiscard)
      )
  }

  def kong(tileToKong: Tile, tilePicked:Tile, tileToDiscard: Tile): Player => Player = {
    (player: Player) =>
      player.copy(
        displayedTiles = player.displayedTiles ++ Seq(Kong(tileToKong)),
        handTile = (player.handTile ++ Seq(tilePicked)) subtract Seq(tileToKong, tileToKong, tileToKong, tileToDiscard)
      )
  }
}

final case class PlayerId(name: String)
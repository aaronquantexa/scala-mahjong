package lotty.core.game

import lotty.core.model.Tile
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.Random

class MalaysianVariantGameRepoTest extends AnyFlatSpec with should.Matchers {

  val startingTiles: Seq[Tile] = Random.shuffle(MalaysianVariantGameRepo.TilesEnumerated)

  val initialisedGame: MalaysianVariantGameRepo = MalaysianVariantGameRepo.initialise(
    PlayerId("a"),
    PlayerId("b"),
    PlayerId("c"),
    startingTiles
  )

  "MalaysianVariantGameRepo" should "distribute tiles across all players without dropping any" in {

    val tiles = (initialisedGame.playerId1.handTile
      ++ initialisedGame.playerId2.handTile
      ++ initialisedGame.playerId3.handTile
      ++ initialisedGame.pickStack
      ++ initialisedGame.playerId1.displayedTiles
      ++ initialisedGame.playerId2.displayedTiles
      ++ initialisedGame.playerId3.displayedTiles)

    tiles.size should equal(startingTiles.size)
  }
}

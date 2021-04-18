package lotty.core.game

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import lotty.core.model.Util

class MalaysianVariantGameTest extends AnyFlatSpec with should.Matchers {

  val initialisedGame: MalaysianVariantGame = MalaysianVariantGame.initialise(
    PlayerId("a"),
    PlayerId("b"),
    PlayerId("c")
  )

  "MalaysianVariantGame" should "distribute tiles across all players without dropping any" in {

    val standardTiles = MalaysianVariantGame.TilesEnumerated.sortBy(Util.circlesFirstSort)

    val tiles = (initialisedGame.playerId1.handTile
      ++ initialisedGame.playerId2.handTile
      ++ initialisedGame.playerId3.handTile
      ++ initialisedGame.pickStack
      ++ initialisedGame.playerId1.displayedFlowers
      ++ initialisedGame.playerId2.displayedFlowers
      ++ initialisedGame.playerId3.displayedFlowers).sortBy(Util.circlesFirstSort)

    tiles should equal(standardTiles)
  }
}

package lotty.core.game

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import lotty.core.model.{CircleTile, Green, Red, White}

class PlayerTest extends AnyFlatSpec with should.Matchers {

  "Player" should "chow correctly" in {

    val player = Player(
      playerId = PlayerId("test"),
      displayedTiles = Seq(Red, Red, Red),
      handTile = Seq(CircleTile(5), CircleTile(6), Green, White)
    )

    val postChowPlayer = player.chow(CircleTile(5), CircleTile(6), CircleTile(7), Green)

    postChowPlayer should equal(Player(
      playerId = PlayerId("test"),
      displayedTiles = Seq(Red, Red, Red, CircleTile(5), CircleTile(6), CircleTile(7)),
      handTile = Seq(White)
    )
    )
  }

  it should "pung correctly" in {
    val player = Player(
      playerId = PlayerId("test"),
      displayedTiles = Seq(Red, Red, Red),
      handTile = Seq(CircleTile(5), CircleTile(5), Green, White)
    )

    val postPungPlayer = player.pung(CircleTile(5), CircleTile(5), Green)

    postPungPlayer should equal(Player(
      playerId = PlayerId("test"),
      displayedTiles = Seq(Red, Red, Red, CircleTile(5), CircleTile(5), CircleTile(5)),
      handTile = Seq(White)
    )
    )
  }

}

package lotty.core.game

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import lotty.core.model.{Chow, CircleTile, Green, Kong, Pung, Red, White}

class PlayerTest extends AnyFlatSpec with should.Matchers {

  "Player" should "chow with 5,6 to form a set 5,6,7." in {

    val player = Player(
      playerId = PlayerId("test"),
      displayedFlowers = Seq.empty,
      displayedTiles = Seq(Pung(Red)),
      handTile = Seq(CircleTile(5), CircleTile(6), Green, White)
    )

    val postChowPlayer = player.chow(CircleTile(5), CircleTile(6), CircleTile(7), Green)

    postChowPlayer should equal(Player(
      playerId = PlayerId("test"),
      displayedFlowers = Seq.empty,
      displayedTiles = Seq(Pung(Red), Chow(CircleTile(5), CircleTile(6), CircleTile(7))),
      handTile = Seq(White)
    )
    )
  }

  it should "pung a 5, to form a set 5,5,5" in {
    val player = Player(
      playerId = PlayerId("test"),
      displayedFlowers = Seq.empty,
      displayedTiles = Seq(Pung(Red)),
      handTile = Seq(CircleTile(5), CircleTile(5), Green, White)
    )

    val postPungPlayer = player.pung(CircleTile(5), Green)

    postPungPlayer should equal(Player(
      playerId = PlayerId("test"),
      displayedFlowers = Seq.empty,
      displayedTiles = Seq(Pung(Red), Pung(CircleTile(5))),
      handTile = Seq(White)
    )
    )
  }

  it should "kong a 5, to form a set 5,5,5,5" in {
    val player = Player(
      playerId = PlayerId("test"),
      displayedFlowers = Seq.empty,
      displayedTiles = Seq.empty,
      handTile = Seq(CircleTile(5), CircleTile(5), CircleTile(5), Green, White)
    )

    val postKongPlayer = player.kong(CircleTile(5), Red, Green)

    postKongPlayer should equal(Player(
      playerId = PlayerId("test"),
      displayedFlowers = Seq.empty,
      displayedTiles = Seq(Kong(CircleTile(5))),
      handTile = Seq(White, Red)
    )
    )
  }

}

package lotty.core.game

import java.util.logging.Logger

import lotty.core.model._

final case class MalaysianVariantGame(
                                    playerId1: Player,
                                    playerId2: Player,
                                    playerId3: Player,
                                    discardedTiles: Seq[Tile],
                                    pickStack: Seq[Tile]
                                  ) {
  def chow(playerId: PlayerId, tile1ToChowWith: Tile, tile2ToChowWith: Tile, tileToDiscard: Tile): MalaysianVariantGame = {
    val updateDiscards = takeActionablePieceAndDiscard(tileToDiscard)

    val updatePlayer = updatePlayerPieces(playerId, Player.chow(tile1ToChowWith,tile2ToChowWith,actionableTile(),tileToDiscard))

    (updateDiscards andThen updatePlayer)(this)
  }

  def pung(playerId: PlayerId, tileToPung: Tile, tileToDiscard: Tile): MalaysianVariantGame = {
    val updateDiscards = takeActionablePieceAndDiscard(tileToDiscard)

    val updatePlayer = updatePlayerPieces(playerId, Player.pung(tileToPung,tileToDiscard))

    (updateDiscards andThen updatePlayer)(this)
  }

  def kong(playerId: PlayerId, tileToKong: Tile, tileToDiscard: Tile): MalaysianVariantGame = {
    val updateDiscards = takeActionablePieceAndDiscard(tileToDiscard)

    val updatePlayer = updatePlayerPieces(playerId, Player.kong(tileToKong, pickStack.head, tileToDiscard))

    (updateDiscards andThen updatePlayer)(this)
  }

  private def updatePlayerPieces(playerId: PlayerId, action: Player => Player): MalaysianVariantGame => MalaysianVariantGame = {
    (game: MalaysianVariantGame) =>
    if(game.playerId1.playerId == playerId)
      game.copy(
        playerId1 = action(game.playerId1)
      )
    else if(this.playerId2.playerId == playerId)
      game.copy(
        playerId2 = action(game.playerId2)
      )
    else
      game.copy(
        playerId3 = action(game.playerId3)
      )
  }

  private def takeActionablePieceAndDiscard(tileToDiscard: Tile): MalaysianVariantGame => MalaysianVariantGame = {
    (game: MalaysianVariantGame) =>
      game.copy(
        discardedTiles = this.discardedTiles.dropLast() ++ Seq(tileToDiscard)
      )
  }

  def actionableTile(): Tile = discardedTiles.last

}

object MalaysianVariantGame {

  private val logger = Logger.getLogger("MalaysianVariantGame")

  private val flowers: Seq[Tile] = (1 to 4).map(BlueFlower) ++ (1 to 4).map(RedFlower) ++ (1 to 4).map(AnimalFlower) ++ (1 to 4).map(FaceFlower)

  private[core] val TilesEnumerated: Seq[Tile] = {
    val winds = Seq.fill(4)(East) ++ Seq.fill(4)(South) ++ Seq.fill(4)(West) ++ Seq.fill(4)(North)
    val dragons = Seq.fill(4)(Red) ++ Seq.fill(4)(White) ++ Seq.fill(4)(Green)
    val circles = Seq.fill(4)((1 to 9).map(CircleTile)).flatten
    val jokers = Seq.fill(4)(Joker)

    winds ++ dragons ++ circles ++ flowers ++ jokers
  }

  val NumberOfTiles: Int = TilesEnumerated.size

  /** This bounds the number of iterations when replacing flowers */
  val NumberOfFlowers: Int = flowers.size

  def initialise(playerId1: PlayerId,
                 playerId2: PlayerId,
                 playerId3: PlayerId,
                 startingTiles: Seq[Tile]): MalaysianVariantGame = {

    val player1Tiles = startingTiles.take(14)
    val player2Tiles = startingTiles.slice(14, 27)
    val player3Tiles = startingTiles.slice(27, 40)
    val untouchedPickStack = startingTiles.drop(40)

    logger.info("Initial starting tiles before resolving flowers")
    logger.info(player1Tiles.show())
    logger.info(player2Tiles.show())
    logger.info(player3Tiles.show())

    val p1FlowerReplacementSize = player1Tiles.flowers().size
    val p2FlowerReplacementSize = player2Tiles.flowers().size
    val p3FlowerReplacementSize = player3Tiles.flowers().size

    val initialTry: ReplacedFlowers = replaceFlowers(p1FlowerReplacementSize, p2FlowerReplacementSize, p3FlowerReplacementSize, untouchedPickStack)

    val totalFlowerSizeInHands: Int = p1FlowerReplacementSize + p2FlowerReplacementSize + p3FlowerReplacementSize
    val pickStack: Seq[Tile] = untouchedPickStack.take(untouchedPickStack.size - totalFlowerSizeInHands)

    val ReplacedNestedFlowers(replacedFlowers, pickStackAfterFlowers) = replaceNestedFlowers(NumberOfFlowers, initialTry, pickStack)

    val (player1TilesInHand, player1DisplayFlowers) = (player1Tiles ++ replacedFlowers.p1).partitionByFlower()

    val (player2TilesInHand, player2DisplayFlowers) = (player2Tiles ++ replacedFlowers.p2).partitionByFlower()

    val (player3TilesInHand, player3DisplayFlowers) = (player3Tiles ++ replacedFlowers.p3).partitionByFlower()

    MalaysianVariantGame(
      playerId1 = Player(
        playerId1,
        player1DisplayFlowers,
        Seq.empty[Meld],
        player1TilesInHand
      ),
      playerId2 = Player(
        playerId2,
        player2DisplayFlowers,
        Seq.empty[Meld],
        player2TilesInHand
      ),
      playerId3 = Player(
        playerId3,
        player3DisplayFlowers,
        Seq.empty[Meld],
        player3TilesInHand
      ),
      discardedTiles = Seq.empty,
      pickStackAfterFlowers
    )

  }

  /**
   * @param p1        Number of tiles to be replaced for player 1
   * @param p2        Number of tiles to be replaced for player 2
   * @param p3        Number of tiles to be replaced for player 3
   * @param pickStack The pick stack
   * @return Tiles, including any extra flowers, from flower replacement
   */
  private[core] def replaceFlowers(p1: Int,
                                   p2: Int,
                                   p3: Int,
                                   pickStack: Seq[Tile]): ReplacedFlowers = {

    val flowerEndOfStack = pickStack.reverse

    val p1FlowerReplacements = flowerEndOfStack.take(p1)
    val p2FlowerReplacements = flowerEndOfStack.slice(p1, p1 + p2)
    val p3FlowerReplacements = flowerEndOfStack.slice(p1 + p2, p1 + p2 + p3)

    ReplacedFlowers(p1FlowerReplacements, p2FlowerReplacements, p3FlowerReplacements)

  }

  def replaceNestedFlowers(numberOfFlowersTotal: Int, firstFlowerReplacement: ReplacedFlowers, pickStack: Seq[Tile]): ReplacedNestedFlowers = {

    val state = NumberOfNestedFlowersState(firstFlowerReplacement.p1.flowers().size, firstFlowerReplacement.p2.flowers().size, firstFlowerReplacement.p3.flowers().size)

    val (replacedFlowers, pickStackAfterFlowers, _) = (1 to NumberOfFlowers).foldLeft((firstFlowerReplacement, pickStack, state)) {
      case ((replacedFlowers, pickStackAfterFlowers, state), _) =>

        val newReplacements: ReplacedFlowers = replaceFlowers(state.p1, state.p2, state.p3, pickStackAfterFlowers)

        val totalNestedFlowerSize: Int = state.p1 + state.p2 + state.p3

        val pickStack = pickStackAfterFlowers.take(pickStackAfterFlowers.size - totalNestedFlowerSize)

        val p1NumberOfFlowersToTakeNextIteration = newReplacements.p1.flowers().size
        val p2NumberOfFlowersToTakeNextIteration = newReplacements.p2.flowers().size
        val p3NumberOfFlowersToTakeNextIteration = newReplacements.p3.flowers().size

        val newState = NumberOfNestedFlowersState(p1NumberOfFlowersToTakeNextIteration, p2NumberOfFlowersToTakeNextIteration, p3NumberOfFlowersToTakeNextIteration)

        (replacedFlowers combine newReplacements, pickStack, newState)
    }

    ReplacedNestedFlowers(replacedFlowers, pickStackAfterFlowers)

  }

}

final case class ReplacedFlowers(
                                  p1: Seq[Tile],
                                  p2: Seq[Tile],
                                  p3: Seq[Tile]
                                ) {

  def combine(replacedFlowers: ReplacedFlowers): ReplacedFlowers = {
    this.copy(
      p1 = this.p1 ++ replacedFlowers.p1,
      p2 = this.p2 ++ replacedFlowers.p2,
      p3 = this.p3 ++ replacedFlowers.p3
    )

  }
}

private final case class NumberOfNestedFlowersState(
                                             p1: Int,
                                             p2: Int,
                                             p3: Int,
                                           )

final case class ReplacedNestedFlowers(
                                        replacedFlowers: ReplacedFlowers,
                                        remainingPickStackAfterFlowers: Seq[Tile]
                                      )

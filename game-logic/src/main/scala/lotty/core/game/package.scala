package lotty.core

import lotty.core.model.{Flower, Tile}

package object game {

  implicit class Tiles(tiles: Seq[Tile]) {
    def show(): String = {
      "|" + tiles.map(_.show).mkString("|") + "|"
    }

    def flowers(): Seq[Tile] = {
      tiles.filter {
        case _: Flower => true
        case _ => false
      }
    }

    def notFlowers(): Seq[Tile] = {
      tiles.filter {
        case _: Flower => false
        case _ => true
      }
    }

    /**
     * @return Non-flowers as first element of tuple, flowers as second element
     */
    def partitionByFlower(): (Seq[Tile], Seq[Flower]) = {

      tiles.foldLeft((Seq.empty[Tile], Seq.empty[Flower])){
        case ((nonFlowers, flowers), flower: Flower) =>
          (nonFlowers, flowers ++ Seq(flower))
        case ((nonFlowers, flowers), nonFlowerTile) =>
          (nonFlowers ++ Seq(nonFlowerTile), flowers)
      }

    }

    /**
     * Subtraction operator
     *
     * @param rightHandSide Tiles
     * @return tiles - rightHandSide
     */
    def subtract(rightHandSide: Seq[Tile]): Seq[Tile] = {
      rightHandSide.foldLeft(tiles) {
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
  }
}

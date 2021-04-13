package lotty.core.model

object Util {
  def circlesFirstSort(tile: Tile): Int = {
    tile match {
      case CircleTile(num) => num
      case Joker => 10
      case Green => 11
      case Red => 12
      case White => 13
      case East => 14
      case South => 15
      case West => 17
      case North => 18
      case _: Flower => 20
    }
  }
}

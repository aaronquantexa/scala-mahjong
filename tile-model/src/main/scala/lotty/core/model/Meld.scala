package lotty.core.model

trait Meld {
  def tiles: Seq[Tile]
}

case class Chow(tile1: Tile, tile2: Tile, tile3: Tile) extends Meld {
  def tiles: Seq[Tile] = Seq(tile1, tile2, tile3)
}

case class Pung(tile: Tile) extends Meld {
  def tiles: Seq[Tile] = Seq.fill(3)(tile)
}

case class Kong(tile: Tile) extends Meld {
  def tiles: Seq[Tile] = Seq.fill(4)(tile)
}

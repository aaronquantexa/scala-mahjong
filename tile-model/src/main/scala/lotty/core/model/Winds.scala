package lotty.core.model

sealed trait Winds extends Tile

case object East extends Winds {
  def show: String = "East"
}

case object South extends Winds {
  def show: String = "South"
}

case object West extends Winds {
  def show: String = "West"
}

case object North extends Winds {
  def show: String = "North"
}
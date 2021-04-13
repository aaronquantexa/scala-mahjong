package lotty.core.model

sealed trait Dragons extends Tile

case object Red extends Dragons {
  def show: String = "RedDragon"
}

case object White extends Dragons {
  def show: String = "WhiteDragon"
}

case object Green extends Dragons {
  def show: String = "GreenDragon"
}

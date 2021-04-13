package lotty.core.model

sealed trait Flower extends Tile

final case class BlueFlower(value: Int) extends Flower {
  def show: String = s"${value.toString} BlueFlower"
}

final case class RedFlower(value: Int) extends Flower {
  def show: String = s"${value.toString} RedFlower"
}

final case class AnimalFlower(value: Int) extends Flower {
  def show: String = s"${value.toString} AnimalFlower"
}

final case class FaceFlower(value: Int) extends Flower {
  def show: String = s"${value.toString} FaceFlower"
}
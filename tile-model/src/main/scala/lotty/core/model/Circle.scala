package lotty.core.model

trait Circle extends Tile

final case class CircleTile(value: Int) extends Circle {
  def show: String = s"${value.toString} Circle"
}





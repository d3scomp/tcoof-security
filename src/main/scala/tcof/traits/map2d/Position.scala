package tcof.traits.map2d

case class Position(x: Double, y: Double) {
  def distanceTo(other: Position): Double = math.sqrt(math.pow(x - other.x, 2) + math.pow(y - other.y, 2))
}

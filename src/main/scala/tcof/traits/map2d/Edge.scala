package tcof.traits.map2d

class Edge[NodeStatusType] private[map2d](val map: Map2D[NodeStatusType], val from: Node[NodeStatusType], val to: Node[NodeStatusType], private var _cost: Double) {
  def cost = _cost
  def cost_=(value: Double) = {
    _cost = value
    map.shortestPath.invalidateCache()
  }
}

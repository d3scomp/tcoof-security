package tcof.traits.map2d

class Edge[NodeDataType] private[map2d](val map: Map2D[NodeDataType], val from: Node[NodeDataType], val to: Node[NodeDataType], private var _cost: Double) {
  def cost = _cost
  def cost_=(value: Double) = {
    _cost = value
    map.shortestPath.invalidateCache()
  }
}

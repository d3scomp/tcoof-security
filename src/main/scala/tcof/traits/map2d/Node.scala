package tcof.traits.map2d

class Node[NodeStatusType] private[map2d](val map: Map2D[NodeStatusType], val center: Position) {
  private[map2d] var _outNeighbors = Map.empty[Node[NodeStatusType], Edge[NodeStatusType]]
  private[map2d] var _inNeighbors = Map.empty[Node[NodeStatusType], Edge[NodeStatusType]]

  def outNeighbors: Map[Node[NodeStatusType], Edge[NodeStatusType]] = _outNeighbors
  def inNeighbors: Map[Node[NodeStatusType], Edge[NodeStatusType]] = _inNeighbors

  var lastVisitTime = Int.MinValue

  var status: NodeStatusType = _

  override def toString() = s"Node(${center.x}, ${center.y})"
}

package tcof.traits.map2d

class Node[NodeDataType] private[map2d](val map: Map2D[NodeDataType], val center: Position) {
  private[map2d] var _outNeighbors = Map.empty[Node[NodeDataType], Edge[NodeDataType]]
  private[map2d] var _inNeighbors = Map.empty[Node[NodeDataType], Edge[NodeDataType]]

  def outNeighbors: Map[Node[NodeDataType], Edge[NodeDataType]] = _outNeighbors
  def inNeighbors: Map[Node[NodeDataType], Edge[NodeDataType]] = _inNeighbors

  var data: NodeDataType = _

  override def toString() = s"Node(${center.x}, ${center.y})"
}

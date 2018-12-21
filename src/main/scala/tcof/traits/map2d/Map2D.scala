package tcof.traits.map2d

class Map2D[NodeDataType] extends WithShortestPath[NodeDataType] {
  private var _nodes = List.empty[Node[NodeDataType]]
  private var _edges = List.empty[Edge[NodeDataType]]

  def nodes: List[Node[NodeDataType]] = _nodes

  def edges: List[Edge[NodeDataType]] = _edges

  def addNode(center: Position): Node[NodeDataType] = {
    val node = new Node(this, center)
    _nodes = _nodes :+ node
    node
  }

  def addDirectedEdge(from: Node[NodeDataType], to: Node[NodeDataType]): Edge[NodeDataType] = from._outNeighbors.get(to) match {
    case Some(edge) => edge

    case None =>
      val edge = new Edge(this, from, to, from.center.distanceTo(to.center))
      _edges = _edges :+ edge
      from._outNeighbors = from._outNeighbors + (to -> edge)
      to._inNeighbors = to._inNeighbors + (from -> edge)
      edge
  }
}

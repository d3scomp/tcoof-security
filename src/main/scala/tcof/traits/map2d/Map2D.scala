package tcof.traits.map2d

class Map2D[NodeStatusType] extends WithShortestPath[NodeStatusType] with WithAreaExploration[NodeStatusType] {
  private var _nodes = List.empty[Node[NodeStatusType]]
  private var _edges = List.empty[Edge[NodeStatusType]]

  def nodes: List[Node[NodeStatusType]] = _nodes

  def edges: List[Edge[NodeStatusType]] = _edges

  def addNode(center: Position): Node[NodeStatusType] = {
    val node = new Node(this, center)
    _nodes = _nodes :+ node
    node
  }

  def addDirectedEdge(from: Node[NodeStatusType], to: Node[NodeStatusType], cost: Double): Edge[NodeStatusType] = from._outNeighbors.get(to) match {
    case Some(edge) => edge

    case None =>
      val edge = new Edge(this, from, to, cost)
      _edges = _edges :+ edge
      from._outNeighbors = from._outNeighbors + (to -> edge)
      to._inNeighbors = to._inNeighbors + (from -> edge)
      edge
  }
}

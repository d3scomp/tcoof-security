package tcof.traits.map2d

import de.ummels.prioritymap.PriorityMap

import scala.collection.mutable

trait WithShortestPath[NodeStatusType] {
  this: Map2D[NodeStatusType] =>

  object shortestPath {
    private[WithShortestPath] val outCache = mutable.Map.empty[Node[NodeStatusType], (List[Node[NodeStatusType]], Map[Node[NodeStatusType], Double], Map[Node[NodeStatusType], Node[NodeStatusType]])]
    private[WithShortestPath] val inCache = mutable.Map.empty[Node[NodeStatusType], (List[Node[NodeStatusType]], Map[Node[NodeStatusType], Double], Map[Node[NodeStatusType], Node[NodeStatusType]])]
    private[WithShortestPath] var epoch = 0

    def invalidateCache(): Unit = {
      synchronized {
        outCache.clear()
        inCache.clear()
        epoch = epoch + 1
      }
    }

    def from(source: Node[NodeStatusType]): ShortestPathFrom = new ShortestPathFrom(source)
    def to(destination: Node[NodeStatusType]): ShortestPathTo = new ShortestPathTo(destination)
  }

  class ShortestPathFrom(source: Node[NodeStatusType]) extends ShortestPath(source) {
    private[WithShortestPath] def getNeighborsWithCosts(node: Node[NodeStatusType]) = node.outNeighbors.values.map(edge => (edge.to, edge.cost))
    private[WithShortestPath] def cache = shortestPath.outCache

    def costTo(destination: Node[NodeStatusType]): Option[Double] = cost(destination)
    def pathTo(destination: Node[NodeStatusType]) = path(destination)
  }

  class ShortestPathTo(destination: Node[NodeStatusType]) extends ShortestPath(destination) {
    private[WithShortestPath] def getNeighborsWithCosts(node: Node[NodeStatusType]) = node.inNeighbors.values.map(edge => (edge.from, edge.cost))
    private[WithShortestPath] def cache = shortestPath.inCache

    def costFrom(source: Node[NodeStatusType]): Option[Double] = cost(source)
    def pathFrom(source: Node[NodeStatusType]) = path(source)
  }

  abstract class ShortestPath(val origin: Node[NodeStatusType]) {
    val (nodesByDistance, distances, predecessors) = compute(origin)

    private[WithShortestPath] def getNeighborsWithCosts(node: Node[NodeStatusType]): Iterable[(Node[NodeStatusType], Double)]
    private[WithShortestPath] def cache: mutable.Map[Node[NodeStatusType], (List[Node[NodeStatusType]], Map[Node[NodeStatusType], Double], Map[Node[NodeStatusType], Node[NodeStatusType]])]

    // Adapted from https://github.com/ummels/dijkstra-in-scala/blob/master/src/main/scala/de/ummels/dijkstra/DijkstraPriority.scala
    // Original version - Copyright (c) 2015, Michael Ummels <michael@ummels.de>
    private def compute(origin: Node[NodeStatusType]): (List[Node[NodeStatusType]], Map[Node[NodeStatusType], Double], Map[Node[NodeStatusType], Node[NodeStatusType]]) = {
      def go(active: PriorityMap[Node[NodeStatusType], Double], nodesByDistance: List[Node[NodeStatusType]], distances: Map[Node[NodeStatusType], Double], predecessors: Map[Node[NodeStatusType], Node[NodeStatusType]]):
      (List[Node[NodeStatusType]], Map[Node[NodeStatusType], Double], Map[Node[NodeStatusType], Node[NodeStatusType]]) =
        if (active.isEmpty)
          (nodesByDistance.reverse.tail, distances, predecessors)
        else {
          val (node, cost) = active.head
          val neighbours = (for {
            (neigh, neighCost) <- getNeighborsWithCosts(node)
            if !distances.contains(neigh) && cost + neighCost < active.getOrElse(neigh, Double.MaxValue)
          } yield neigh -> (cost + neighCost)) toMap

          val preds = neighbours mapValues (_ => node)
          go(active.tail ++ neighbours, node :: nodesByDistance, distances + (node -> cost), predecessors ++ preds)
        }

      var result: (List[Node[NodeStatusType]], Map[Node[NodeStatusType], Double], Map[Node[NodeStatusType], Node[NodeStatusType]]) = null
      var epoch = 0

      synchronized {
        cache.get(origin) match {
          case Some(x) => result = x
          case None =>
        }

        epoch = shortestPath.epoch
      }

      if (result == null) {
        result = go(PriorityMap(origin -> 0), List.empty[Node[NodeStatusType]], Map.empty[Node[NodeStatusType], Double], Map.empty[Node[NodeStatusType], Node[NodeStatusType]])

        synchronized {
          if (shortestPath.epoch == epoch)
            cache += (origin -> result)
        }
      }

      result
    }

    private[WithShortestPath] def cost(target: Node[NodeStatusType]): Option[Double] = distances.get(target)

    private[WithShortestPath] def path(target: Node[NodeStatusType]) = {
      def go(current: Node[NodeStatusType], pathSoFar: List[Node[NodeStatusType]] = List()): List[Node[NodeStatusType]] = {
        predecessors.get(current) match {
          case None => pathSoFar
          case Some(node) => go(node, current :: pathSoFar)
        }
      }

      if (origin == target)
        Some(List())
      else if (predecessors.contains(target))
        Some(go(target))
      else
        None
    }
  }

}

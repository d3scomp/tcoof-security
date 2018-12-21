package tcof.traits.map2d

import de.ummels.prioritymap.PriorityMap

import scala.collection.mutable

trait WithShortestPath[NodeDataType] {
  this: Map2D[NodeDataType] =>

  object shortestPath {
    private[WithShortestPath] val outCache = mutable.Map.empty[Node[NodeDataType], (List[Node[NodeDataType]], Map[Node[NodeDataType], Double], Map[Node[NodeDataType], Node[NodeDataType]])]
    private[WithShortestPath] val inCache = mutable.Map.empty[Node[NodeDataType], (List[Node[NodeDataType]], Map[Node[NodeDataType], Double], Map[Node[NodeDataType], Node[NodeDataType]])]
    private[WithShortestPath] var epoch = 0

    def invalidateCache(): Unit = {
      synchronized {
        outCache.clear()
        inCache.clear()
        epoch = epoch + 1
      }
    }

    def from(source: Node[NodeDataType]): ShortestPathFrom = new ShortestPathFrom(source)
    def to(destination: Node[NodeDataType]): ShortestPathTo = new ShortestPathTo(destination)
  }

  class ShortestPathFrom(source: Node[NodeDataType]) extends ShortestPath(source) {
    private[WithShortestPath] def getNeighborsWithCosts(node: Node[NodeDataType]) = node.outNeighbors.values.map(edge => (edge.to, edge.cost))
    private[WithShortestPath] def cache = shortestPath.outCache

    def costTo(destination: Node[NodeDataType]): Option[Double] = cost(destination)
    def pathTo(destination: Node[NodeDataType]) = path(destination)
  }

  class ShortestPathTo(destination: Node[NodeDataType]) extends ShortestPath(destination) {
    private[WithShortestPath] def getNeighborsWithCosts(node: Node[NodeDataType]) = node.inNeighbors.values.map(edge => (edge.from, edge.cost))
    private[WithShortestPath] def cache = shortestPath.inCache

    def costFrom(source: Node[NodeDataType]): Option[Double] = cost(source)
    def pathFrom(source: Node[NodeDataType]) = path(source)
  }

  abstract class ShortestPath(val origin: Node[NodeDataType]) {
    val (nodesByDistance, distances, predecessors) = compute(origin)

    private[WithShortestPath] def getNeighborsWithCosts(node: Node[NodeDataType]): Iterable[(Node[NodeDataType], Double)]
    private[WithShortestPath] def cache: mutable.Map[Node[NodeDataType], (List[Node[NodeDataType]], Map[Node[NodeDataType], Double], Map[Node[NodeDataType], Node[NodeDataType]])]

    // Adapted from https://github.com/ummels/dijkstra-in-scala/blob/master/src/main/scala/de/ummels/dijkstra/DijkstraPriority.scala
    // Original version - Copyright (c) 2015, Michael Ummels <michael@ummels.de>
    private def compute(origin: Node[NodeDataType]): (List[Node[NodeDataType]], Map[Node[NodeDataType], Double], Map[Node[NodeDataType], Node[NodeDataType]]) = {
      def go(active: PriorityMap[Node[NodeDataType], Double], nodesByDistance: List[Node[NodeDataType]], distances: Map[Node[NodeDataType], Double], predecessors: Map[Node[NodeDataType], Node[NodeDataType]]):
      (List[Node[NodeDataType]], Map[Node[NodeDataType], Double], Map[Node[NodeDataType], Node[NodeDataType]]) =
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

      var result: (List[Node[NodeDataType]], Map[Node[NodeDataType], Double], Map[Node[NodeDataType], Node[NodeDataType]]) = null
      var epoch = 0

      synchronized {
        cache.get(origin) match {
          case Some(x) => result = x
          case None =>
        }

        epoch = shortestPath.epoch
      }

      if (result == null) {
        result = go(PriorityMap(origin -> 0), List.empty[Node[NodeDataType]], Map.empty[Node[NodeDataType], Double], Map.empty[Node[NodeDataType], Node[NodeDataType]])

        synchronized {
          if (shortestPath.epoch == epoch)
            cache += (origin -> result)
        }
      }

      result
    }

    private[WithShortestPath] def cost(target: Node[NodeDataType]): Option[Double] = distances.get(target)

    private[WithShortestPath] def path(target: Node[NodeDataType]) = {
      def go(current: Node[NodeDataType], pathSoFar: List[Node[NodeDataType]] = List()): List[Node[NodeDataType]] = {
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

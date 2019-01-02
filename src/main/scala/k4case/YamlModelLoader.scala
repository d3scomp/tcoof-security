package k4case

import tcof.traits.map2d.{Map2D, Node}

import scala.collection.mutable

trait YamlModelLoader {
  this: TestScenario =>

  def loadYamlModel(modelPath: String) = {
    import tcof.YamlLoader._

    val model: YamlDict = loadYaml("model.yaml")

    val workers = for {
      workerYaml <- model("employees").asList[YamlDict]

    } yield new Worker(
      id = workerYaml("id"),
      position = workerYaml("position"),
      capabilities = workerYaml("capabilities")
    )


    val factories = for {
      factoryYaml <- model("factories").asList[YamlDict]

      factoryId = factoryYaml("id"): String

      workPlaces = for {
        workPlaceYaml <- factoryYaml("workPlaces").asList[YamlDict]
        workPlaceId = workPlaceYaml("id"): String

      } yield new WorkPlace(
        id = workPlaceId,
        position = workPlaceYaml("position"),
        entryDoor = new Door(workPlaceId, workPlaceYaml("door"))
      )

    } yield new Factory(
      id = factoryId,
      position = factoryYaml("position"),
      entryDoor = new Door(factoryId + "_door", factoryYaml("door")),
      dispenser = new Dispenser(factoryId + "_dispenser", factoryYaml("dispenser")),
      workPlaces = workPlaces
    )


    val workersMap = Map(workers.map(x => x.id -> x) : _*)
    val workPlacesMap = Map(factories.flatMap(_.workPlaces.map(x => x.id -> x)) : _*)

    val shifts = for {
      shiftYaml <- model("shifts").asList[YamlDict]

      assignmentPairs = shiftYaml("assignment").asList[YamlDict].map(
        asgn => workersMap(asgn("worker")) -> (asgn("capability"): String)
      )

    } yield new Shift(
      id = shiftYaml("id"),
      startTime = shiftYaml("startsAt"),
      endTime = shiftYaml("endsAt"),
      workPlace = workPlacesMap(shiftYaml("workPlace")),
      foreman = workersMap(shiftYaml("foreman")),
      workers = shiftYaml("workers").asList[String].map(workersMap(_)),
      standbys = shiftYaml("standbys").asList[String].map(workersMap(_)),
      assignments = Map[Worker, String](assignmentPairs: _*)
    )


    val map = new Map2D[MapNodeData]
    val mapYaml = model("map").asMap

    val mapNodes = mutable.Map.empty[String, Node[MapNodeData]]
    for (pointYaml <- mapYaml("points").asList[YamlDict]) {
      val node = map.addNode(pointYaml("position"))
      node.data = MapNodeData(pointYaml("id"))
      mapNodes(node.data.id) = node
    }

    for (edgeYaml <- mapYaml("edges").asList[YamlDict]) {
      val fromNode = mapNodes(edgeYaml("nodeOut"))
      val toNode = mapNodes(edgeYaml("nodeIn"))

      map.addDirectedEdge(fromNode, toNode)

      if (edgeYaml("bidirectional")) {
        map.addDirectedEdge(toNode, fromNode)
      }
    }

    (map, workers, factories, shifts)
  }
}

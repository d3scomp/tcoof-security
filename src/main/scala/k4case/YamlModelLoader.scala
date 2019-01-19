package k4case

/*
import tcof.traits.map2d.{Map2D, Node, Position}

import scala.collection.mutable

trait YamlModelLoader {
  this: TestScenario =>

  def loadYamlModel(modelPath: String) = {
    import tcof.YamlLoader._

    val modelYaml: YamlDict = loadYaml("model.yaml")

    val workers = for {
      workerYaml <- modelYaml("employees").asList[YamlDict]

    } yield new Worker(
      id = workerYaml("id"),
      position = workerYaml("position"),
      capabilities = workerYaml("capabilities")
    )

    val workersMap = Map(workers.map(x => x.id -> x) : _*)


    val factories = for {
      factoryYaml <- modelYaml("factories").asList[YamlDict]

      factoryId = factoryYaml("id"): String

      workPlaces = for {
        workPlaceYaml <- factoryYaml("workPlaces").asList[YamlDict]
        workPlaceId = workPlaceYaml("id"): String

      } yield new WorkPlace(
        id = workPlaceId,
        positions = List(workPlaceYaml("position")),
        entryDoor = new Door(workPlaceId + "_door", workPlaceYaml("door"))
      )

    } yield new Factory(
      id = factoryId,
      positions = factoryYaml("positions").asList,
      entryDoor = new Door(factoryId + "_door", factoryYaml("door")),
      dispenser = new Dispenser(factoryId + "_dispenser", factoryYaml("dispenser")),
      workPlaces = workPlaces
    )

    val factoriesMap = Map(factories.map(x => x.id -> x) : _*)
    val workPlacesMap = Map(factories.flatMap(_.workPlaces.map(x => x.id -> x)) : _*)


    val shifts = for {
      shiftYaml <- modelYaml("shifts").asList[YamlDict]

      assignmentPairs = shiftYaml("assignment").asList[YamlDict].map(
        asgn => workersMap(asgn("worker")) -> (asgn("capability"): String)
      )

    } yield new Shift(
      id = shiftYaml("id"),
      startTime = shiftYaml("startsAt"),
      endTime = shiftYaml("endsAt"),
      workPlace = workPlacesMap(shiftYaml("workPlace")),
      foreman = workersMap(shiftYaml("foreman")),
      workers = shiftYaml("workers").asList.map(workersMap(_)),
      standbys = shiftYaml("standbys").asList.map(workersMap(_)),
      assignments = Map[Worker, String](assignmentPairs: _*)
    )

    val shiftsMap = Map(shifts.map(x => x.id -> x) : _*)


    val map = new Map2D[AnyRef]
    val mapYaml = modelYaml("map").asYamlDict

    val mapNodes = mutable.Map.empty[String, Node[AnyRef]]
    for (pointYaml <- mapYaml("points").asList[YamlDict]) {
      val node = map.addNode(pointYaml("position"))
      mapNodes(pointYaml("id")) = node
    }

    for (edgeYaml <- mapYaml("edges").asList[YamlDict]) {
      val fromNode = mapNodes(edgeYaml("nodeOut"))
      val toNode = mapNodes(edgeYaml("nodeIn"))

      map.addDirectedEdge(fromNode, toNode)

      if (edgeYaml("bidirectional")) {
        map.addDirectedEdge(toNode, fromNode)
      }
    }



    (map, workersMap, factoriesMap, shiftsMap)
  }

}
*/

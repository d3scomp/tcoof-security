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
      workerYaml("id"),
      workerYaml("position"),
      workerYaml("capabilities")
    )


    val factories = for {
      factoryYaml <- model("factories").asList[YamlDict]

      factoryId = factoryYaml("id"): String

      workplaces = for {
        workplaceYaml <- factoryYaml("workplaces").asList[YamlDict]
        workplaceId = workplaceYaml("id"): String

      } yield new WorkPlace(
        workplaceId,
        workplaceYaml("position"),
        new Door(workplaceId, workplaceYaml("door"))
      )

    } yield new Factory(
      factoryId,
      factoryYaml("position"),
      new Door(factoryId + "_door", factoryYaml("door")),
      new Dispenser(factoryId + "_dispenser", factoryYaml("dispenser")),
      workplaces
    )


    val workersMap = Map(workers.map(x => x.id -> x) : _*)
    val workplacesMap = Map(factories.flatMap(_.workplaces.map(x => x.id -> x)) : _*)

    val shifts = for {
      shiftYaml <- model("shifts").asList[YamlDict]

      assignmentPairs = shiftYaml("assignment").asList[YamlDict].map(
        asgn => workersMap(asgn("worker")) -> (asgn("capability"): String)
      )

    } yield new Shift(
      shiftYaml("id"),
      shiftYaml("startsAt"),
      shiftYaml("endsAt"),
      workplacesMap(shiftYaml("workPlace")),
      workersMap(shiftYaml("foreman")),
      shiftYaml("workers").asList[String].map(workersMap(_)),
      shiftYaml("standbys").asList[String].map(workersMap(_)),
      Map[Worker, String](assignmentPairs: _*)
    )


    val map = new Map2D[MapNodeData]
    val mapYaml = model("map").asMap

    val mapNodes = mutable.Map.empty[String, Node[MapNodeData]]
    for (point <- mapYaml("points").asList[YamlDict]) {
      val node = map.addNode(point("position"))
      node.data = MapNodeData(point("id"))
      mapNodes(node.data.id) = node
    }

    for (edge <- mapYaml("edges").asList[YamlDict]) {
      val fromNode = mapNodes(edge("nodeOut"))
      val toNode = mapNodes(edge("nodeIn"))

      map.addDirectedEdge(fromNode, toNode)

      if (edge("bidirectional")) {
        map.addDirectedEdge(toNode, fromNode)
      }
    }

    (map, workers, factories, shifts)
  }
}

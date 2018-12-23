package k4case

import java.nio.file.{Files, Paths}
import java.util
import java.util.{List, Map}

import k4case.TestScenario.Capability.Capability
import org.joda.time.LocalDateTime
import org.yaml.snakeyaml.Yaml
import tcof._
import tcof.traits.map2d.{Map2DTrait, Node, Position}

import scala.util.control.Breaks._

class NodeData

class TestScenario extends Model with Map2DTrait[NodeData] {

  case class HurryUpNotification(shift: Shift)
 // case class

  import TestScenario.Capability._
  //import Notifications._

  class Door(
              val id: String,
              val position: Position
            ) extends Component {
    name(s"Door ${id}")
  }

  class Dispenser(
                 val id: String,
                 val position: Position
                 ) extends Component {
    name(s"Protection equipment dispenser ${id}")
  }

  class Worker(
                val id: String,
                var position: Position,
                val capabilities: Set[String]
              ) extends Component {
    name(s"Worker ${id}")
  }

  class Room (
               val id: String,
               val position: Position,
               val entryDoor: Door
             ) extends Component {
    name(s"Room ${id}")
  }

  class WorkPlace(
                   id: String,
                   position: Position,
                   entryDoor: Door,
                   val inRoom: Room
                 ) extends Room(id, position, entryDoor) {
    name(s"WorkPlace ${id}")
  }

  class Factory (
                  id: String,
                  position: Position,
                  entryDoor: Door,
                  val dispenser: Dispenser
                ) extends Room(id, position, entryDoor) {
    name(s"Factory ${id}")
  }

  class Shift(
               val id: String,
               val startTime: LocalDateTime,
               val endTime: LocalDateTime,
               val workPlace: WorkPlace,
               val foreman: Worker,
               val workers: Array[Worker],
               val standbys: Array[Worker],
               val assignments: Map[Worker, String]
             ) extends Component {
    name(s"Shift ${id}")
  }

  var workers = EntityReader.readWorkersFromYaml(this, "model.yaml", (id, pos, caps) => { new Worker(id, pos, caps)})

  def readShifts(): Unit = {
    val yaml = new Yaml()
    val data: util.Map[String, util.List[util.Map[String, AnyRef]]] = yaml.load(Files.newBufferedReader(Paths.get("model.yaml")))
    val shifts = data.get("shifts")
    shifts.forEach(shift => {
      val id = shift.get("id")
      val startTime = shift.get("startsAt")
      val endTime = shift.get("endsAt")
      val formanName = shift.get("foreman")
      val foreman = workers.toStream.filter(w => w.id.equals(formanName)).head


    })
  }

//    val workplaceA =

//      val
 /* class ShiftTeam(shift: Shift) {
    // These are like invariants at a given point of time
    val cancelledWorkers = shift.workers.filter(wrk => wrk notified Notification(shift.id, ASSIGNMENT_CANCELED_DUE_LATENESS))

    val workersInShift = shift.workers diff cancelledWorkers

    class AccessToTheHall extends Ensemble {
      constraints {
        now >= shift.startTime - 30 minutes &&
        now <= shift.endTime + 30 minutes
      }

      allow(workersInShift, "enter", shift.workPlace)
    }

    object NotificationOfWorkersThatAreLate extends Ensemble {
      val workersThatAreLate = workersInShift.filter(wrk => !(wrk isAt shift.workPlace))

      constraints {
        now >= shift.startTime - 20 minutes
      }

      notify(workersThatAreLate, Notification(shift.id))
    }



  } */

  /*root(new ShiftTeam(shiftA) ) */
}


object TestScenario {

  object Capability extends Enumeration {
    type Capability = Value
    val CAP_A = Value("A")
    val CAP_B = Value("B")
    val CAP_C = Value("C")
    val CAP_D = Value("D")
    val CAP_E = Value("E")
    val CAP_UNKNOWN = Value("UNKNOWN")
  }

  def main(args: Array[String]): Unit = {
    val scenario = new TestScenario
    scenario.init()
    EntityReader.readMapFromYaml(scenario, "model.yaml")
    scenario.readShifts()

    /*
    val components = List(
      new scenario.FireBrigade(1),
      new scenario.FireBrigade(2),
      new scenario.FireBrigade(3),
      new scenario.AmbulanceTeam(1),
      new scenario.AmbulanceTeam(2)
    )

    scenario.components = components

    val mapNodes = new Array[Array[Node[MapNodeStatus]]](10)

    for (x <- 0 until 10) {
      mapNodes(x) = new Array[Node[MapNodeStatus]](10)
      for (y <- 0 until 10) {
        val node = scenario.map.addNode(Position(x, y))
        node.status = new MapNodeStatus(MapNodeKind.Road, false, 0)
        mapNodes(x)(y) = node
      }
    }

    for (x <- 1 until 10) {
      for (y <- 1 until 10) {
        scenario.map.addDirectedEdge(mapNodes(x)(y), mapNodes(x-1)(y), 1)
        scenario.map.addDirectedEdge(mapNodes(x)(y), mapNodes(x)(y-1), 1)
        scenario.map.addDirectedEdge(mapNodes(x-1)(y), mapNodes(x)(y), 1)
        scenario.map.addDirectedEdge(mapNodes(x)(y-1), mapNodes(x)(y), 1)
      }
    }

    val componentMapPositions = List(
      mapNodes(2)(3),
      mapNodes(0)(8),
      mapNodes(1)(4),
      mapNodes(7)(6),
      mapNodes(9)(2)
    )

    components.zip(componentMapPositions).foreach{
      case (component, position) => component.mapPosition = position
    }

    mapNodes(7)(2).data = new MapNodeStatus(MapNodeKind.Building, true, 0.1)
    mapNodes(3)(4).data = new MapNodeStatus(MapNodeKind.Building, true, 0.7)


    val trials = 3
    val dist = new BinomialDistribution(trials, 0.91)

    scenario.noOfStatusMsgToBeReceived = trials
    for (t <- 0 until 5000) {
      scenario.noOfStatusMsgReceived = dist.sample()
      scenario.step(t)
    }


    scenario.rootEnsemble.init()
    println("System initialized")

    while (scenario.rootEnsemble.solve()) {
      println(scenario.rootEnsemble.instance.toString)
    }

    scenario.rootEnsemble.commit()

    println(scenario.rootEnsemble.instance.solutionUtility)
    */
  }

}


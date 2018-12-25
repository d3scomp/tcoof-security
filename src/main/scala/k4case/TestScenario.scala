package k4case

import java.nio.file.{Files, Paths}
import java.util

import org.joda.time.LocalDateTime
import org.yaml.snakeyaml.Yaml
import tcof._
import tcof.traits.map2d.{Map2DTrait, Node, Position}

import scala.collection.JavaConverters._

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

    override def toString = s"Door($id, $position)"
  }

  class Dispenser(
                 val id: String,
                 val position: Position
                 ) extends Component {
    name(s"Protection equipment dispenser ${id}")

    override def toString = s"Dispenser($id, $position)"
  }

  class Worker(
                val id: String,
                var position: Position,
                val capabilities: Set[String]
              ) extends Component {
    name(s"Worker ${id}")

    override def toString = s"Worker($id, $position, $capabilities)"
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

    override def toString = s"WorkPlace($id, $position, $entryDoor, $inRoom)"
  }

  class Factory (
                  id: String,
                  position: Position,
                  entryDoor: Door,
                  val dispenser: Dispenser
                ) extends Room(id, position, entryDoor) {
    name(s"Factory ${id}")

    override def toString = s"Factory($id, $position, $entryDoor, $dispenser)"
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

    override def toString = s"Shift($startTime, $endTime, $workPlace, $foreman, $workers, $standbys, $assignments)"
  }

  val workers = EntityReader.readWorkersFromYaml(this, "model.yaml", (id, pos, caps) => { new Worker(id, pos, caps)}).asInstanceOf[Set[Worker]]

  def readRooms():Set[Room] = {
    val yaml = new Yaml()
    val data: util.Map[String, util.List[util.Map[String, AnyRef]]] = yaml.load(Files.newBufferedReader(Paths.get("model.yaml")))
    val wps = data.get("workplaces")
    val roomsList = scala.collection.mutable.MutableList[Room]()
    wps.forEach(wp => {
      val id = wp.get("id").toString
      val position = EntityReader.readPosition(wp.get("position"))
      val doorPosition = EntityReader.readPosition(wp.get("door"))
      val door = new Door(id, doorPosition)
      val inRoomString = wp.get("inRoom").toString
      val inRoom = roomsList.toStream.filter(rm => rm.id.equals(inRoomString)).headOption
      val dispenserRaw = Option[Object](wp.get("dispenser"))
      val room = if (inRoomString.equals("None")) new Factory(id, position, door, new Dispenser(id, EntityReader.readPosition(dispenserRaw.get))) else new WorkPlace(id, position, door, inRoom.get)
      roomsList += room
    })
    return roomsList.toSet
  }
  val rooms = readRooms()

  def readShifts(): Set[Shift] = {
    val yaml = new Yaml()
    val data: util.Map[String, util.List[util.Map[String, AnyRef]]] = yaml.load(Files.newBufferedReader(Paths.get("model.yaml")))
    val shifts = data.get("shifts")
    val shiftsList = scala.collection.mutable.MutableList[Shift]()
    shifts.forEach(shift => {
      val id = shift.get("id").toString
      val startTime = LocalDateTime.fromDateFields(shift.get("startsAt").asInstanceOf[util.Date])
      val endTime = LocalDateTime.fromDateFields(shift.get("endsAt").asInstanceOf[util.Date])
      val workPlaceName = shift.get("workPlace").toString
      val workPlace = rooms.toStream.filter(rm => rm.id.equals(workPlaceName)).head.asInstanceOf[WorkPlace]
      val formanName = shift.get("foreman").toString
      val foreman = workers.toStream.filter(w => {w.id.equals(formanName)}).head
      val workersNames = shift.get("workers").asInstanceOf[util.List[Object]]
      val shiftWorkers = workersNames.iterator().asScala.toStream.map(name => {
        workers.toStream.filter(w => w.id.equals(name.toString)).head
      }).toArray
      val standbyNames = shift.get("standby").asInstanceOf[util.List[Object]]
      val standby = standbyNames.iterator().asScala.toStream.map(name => {
        workers.toStream.filter(w => w.id.equals(name.toString)).head
      }).toArray
      val assignmentList = shift.get("assignment").asInstanceOf[util.List[util.Map[String, Object]]]
      val assignment = assignmentList.iterator().asScala.toStream.map(wk => {
        val worker = workers.toStream.filter(w => w.id.equals(wk.get("worker").toString)).head
        (worker, wk.get("capability").toString)
      }).toMap
      shiftsList += new Shift(id, startTime, endTime, workPlace, foreman, shiftWorkers, standby, assignment)
    })
    return shiftsList.toSet
  }
  val shifts = readShifts()

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


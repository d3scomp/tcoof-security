package k4case

import java.time.LocalDateTime

import tcof._
import tcof.traits.map2d.{Map2DTrait, Position}


case class MapNodeData(id: String)

class TestScenario extends Model with Map2DTrait[MapNodeData] with YamlModelLoader {
  var now: LocalDateTime = null

  case class HurryUpNotification(shift: Shift) extends Notification
  case class AssignmentCancelledNotification(shift: Shift) extends Notification
  case class CallStandbyNotification(shift: Shift) extends Notification

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

    def isAt(room: Room) = false // FIXME
  }

  abstract class Room(
              val id: String,
              val position: Position,
              val entryDoor: Door
            ) extends Component {
    name(s"Room ${id}")
  }

  class WorkPlace(
                   id: String,
                   position: Position,
                   entryDoor: Door
                 ) extends Room(id, position, entryDoor) {
    name(s"WorkPlace ${id}")

    var factory: Factory = _

    override def toString = s"WorkPlace($id, $position, $entryDoor)"
  }

  class Factory(
                 id: String,
                 position: Position,
                 entryDoor: Door,
                 val dispenser: Dispenser,
                 val workPlaces: List[WorkPlace]
               ) extends Room(id, position, entryDoor) {
    name(s"Factory ${id}")

    for (workPlace <- workPlaces) {
      workPlace.factory = this
    }

    override def toString = s"Factory($id, $position, $entryDoor, $dispenser, $workPlaces)"
  }

  class Shift(
               val id: String,
               val startTime: LocalDateTime,
               val endTime: LocalDateTime,
               val workPlace: WorkPlace,
               val foreman: Worker,
               val workers: List[Worker],
               val standbys: List[Worker],
               val assignments: Map[Worker, String]
             ) extends Component {
    name(s"Shift ${id}")

    override def toString = s"Shift($startTime, $endTime, $workPlace, $foreman, $workers, $standbys, $assignments)"
  }

  val (map, workers, factories, shifts) = loadYamlModel("model.yaml")


  class ShiftTeam(shift: Shift) extends RootEnsemble {
    name(s"Shift team ${shift.id}")

    // These are like invariants at a given point of time
    val cancelledWorkers = shift.workers.filter(wrk => wrk notified AssignmentCancelledNotification(shift))

    val calledInStandbys = shift.standbys.filter(wrk => wrk notified CallStandbyNotification(shift))
    val availableStandbys = shift.standbys diff calledInStandbys

    val assignedWorkers = (shift.workers union calledInStandbys) diff cancelledWorkers


    object AccessToTheHall extends Ensemble { // Kdyz se constraints vyhodnoti na LogicalBoolean, tak ten ensemble vubec nezatahujeme solver modelu a poznamename si, jestli vysel nebo ne
      name(s"AccessToHall")

      situation {
        (now isAfter (shift.startTime minusMinutes 30)) &&
          (now isBefore (shift.endTime plusMinutes 30))
      }

      allow(assignedWorkers, "enter", shift.workPlace)
    }


    object NotificationOfWorkersThatArePotentiallyLate extends Ensemble {
      name(s"NotificationOfWorkersThatArePotentiallyLate")

      val workersThatAreLate = assignedWorkers.filter(wrk => !(wrk isAt shift.workPlace))

      situation {
        now isAfter (shift.startTime minusMinutes 20)
      }

      notify(workersThatAreLate, HurryUpNotification(shift))
    }


    object CancellationOfWorkersThatAreLate extends Ensemble {
      name(s"CancellationOfWorkersThatAreLate")

      val workersThatAreLate = assignedWorkers.filter(wrk => !(wrk isAt shift.workPlace))

      situation {
        now isAfter (shift.startTime minusMinutes 15)
      }

      notify(workersThatAreLate, AssignmentCancelledNotification(shift))
    }


    object AccessToTheDispenser extends Ensemble {
      name(s"AccessToTheDispenser")

      situation {
        (now isAfter (shift.startTime minusMinutes 15)) &&
          (now isBefore shift.endTime)
      }

      allow(assignedWorkers, "use", shift.workPlace.factory.dispenser)
    }

    object AssignmentOfStandbys extends Ensemble {
      name(s"AssignmentOfStandbys")

      class StandbyAssignment(cancelledWorker: Worker) extends Ensemble {
        name(s"StandbyAssignment for ${cancelledWorker.id}")

        val standby = oneOf(availableStandbys union calledInStandbys)

        constraints {
          standby.all(_.capabilities contains shift.assignments(cancelledWorker))
        }

        utility {
          standby.sum(wrk => if (calledInStandbys contains wrk) 1 else 0)
        }
      }

      val standbyAssignments = rules(cancelledWorkers.map(new StandbyAssignment(_)))

      situation {
        (now isAfter (shift.startTime minusMinutes 15)) &&
        (now isBefore shift.endTime)
      }

      constraints {
        standbyAssignments.map(_.standby).allDisjoint
      }

      utility {
        standbyAssignments.sum(_.utility)
      }

    }

    rules(
      AccessToTheHall,
      NotificationOfWorkersThatArePotentiallyLate,
      CancellationOfWorkersThatAreLate,
      AccessToTheDispenser
    )

  }

  val rootEnsemble = root(new ShiftTeam(shifts(0)))
}

object TestScenario {

  def main(args: Array[String]): Unit = {
    val scenario = new TestScenario
    scenario.init()

    scenario.now = LocalDateTime.parse("2018-12-03T09:40:00")

    scenario.rootEnsemble.init()
    println("System initialized")

    while (scenario.rootEnsemble.solve()) {
      println(scenario.rootEnsemble.instance.toString)
    }

    scenario.rootEnsemble.commit()

    println(scenario.rootEnsemble.instance.solutionUtility)
  }

}


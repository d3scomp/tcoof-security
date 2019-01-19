package k4case

import java.io.{File, PrintWriter}
import java.time.LocalDateTime

import tcof._


case class Position(x: Double, y: Double)

class TestScenario extends Model with ModelGenerator {
  val startTimestamp = LocalDateTime.parse("2018-12-03T08:00:00")
  var now = startTimestamp

  case class HurryUpNotification(shift: Shift) extends Notification
  case class AssignmentCancelledNotification(shift: Shift) extends Notification
  case class CallStandbyNotification(shift: Shift) extends Notification

  case class ScenarioEvent(timestamp: LocalDateTime, eventType: String, person: String, position: Position)

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

    def isAt(room: Room) = room.positions.contains(position)
  }

  abstract class Room(
              val id: String,
              val positions: List[Position],
              val entryDoor: Door
            ) extends Component {
    name(s"Room ${id}")
  }

  class WorkPlace(
                   id: String,
                   positions: List[Position],
                   entryDoor: Door
                 ) extends Room(id, positions, entryDoor) {
    name(s"WorkPlace ${id}")

    var factory: Factory = _

    override def toString = s"WorkPlace($id, $positions, $entryDoor)"
  }

  class Factory(
                 id: String,
                 positions: List[Position],
                 entryDoor: Door,
                 val dispenser: Dispenser,
                 val workPlaces: List[WorkPlace]
               ) extends Room(id, positions, entryDoor) {
    name(s"Factory ${id}")

    for (workPlace <- workPlaces) {
      workPlace.factory = this
    }

    override def toString = s"Factory($id, $positions, $entryDoor, $dispenser, $workPlaces)"
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

  /*
  val workersOnTimePerWorkplaceCount = 96
  val workersLatePerWorkplaceCount = 4
  val workersOnStandbyCount = 1000
  val factoriesCount = 50
*/

  val workersOnTimePerWorkplaceCount = 96
  val workersLatePerWorkplaceCount = 4
  val workersOnStandbyCount = 1000
  val factoriesCount = 50

  val factoryIds = (1 to factoriesCount).map(idx => f"factory$idx%02d")

  import ModelDSL._
  val (workersMap, factoriesMap, shiftsMap) = withModel { implicit builder =>
    val workersOnStandby = (1 to workersOnStandbyCount).map(idx => f"standby-$idx%03d")

    for (id <- workersOnStandby) {
      withUnscopedWorker(id, Set("A", "B", "C", "D", "E"))
    }

    for ((factoryId, factoryIdx) <- factoryIds.zipWithIndex) {
      withFactory(factoryId, 0, 0) { implicit scope =>
        for (wp <- List("A", "B", "C")) {
          val foremanId = s"$factoryId-$wp-foreman"
          withWorker(foremanId, Set("A", "B", "C", "D", "E"))

          val workersOnTime = (1 to workersOnTimePerWorkplaceCount).map(idx => f"$factoryId%s-$wp%s-ontime-$idx%03d")
          for (id <- workersOnTime) {
            withWorker(id, Set("A", "B", "C", "D", "E"))
          }

          val workersLate = (1 to workersLatePerWorkplaceCount).map(idx => f"$factoryId%s-$wp%s-late-$idx%03d")
          for (id <- workersLate) {
            withWorker(id, Set("A", "B", "C", "D", "E"))
          }

          val workersInShift = workersOnTime ++ workersLate

          withShift(
            wp,
            startTimestamp plusHours 1,
            startTimestamp plusHours 9,
            wp,
            foremanId,
            workersInShift.toList,
            workersOnStandby.toList.slice(factoryIdx * workersLatePerWorkplaceCount * 3, (factoryIdx + 5) * workersLatePerWorkplaceCount * 3),
            //workersOnStandby.toList,
            workersInShift.map(wrk => (wrk, "A")).toMap
          )
        }
      }
    }
  }

  import EventsDSL._
  val events = withEvents { implicit builder =>
    for (factoryId <- factoryIds) {
      // foremen
      withWorkerInShiftA(s"$factoryId-A-foreman", startTimestamp)
      withWorkerInShiftB(s"$factoryId-B-foreman", startTimestamp)
      withWorkerInShiftC(s"$factoryId-C-foreman", startTimestamp)

      // Workers that are on time
      for (idx <- 1 to workersOnTimePerWorkplaceCount) {
        withWorkerInShiftA(f"$factoryId%s-A-ontime-$idx%03d", startTimestamp)
        withWorkerInShiftB(f"$factoryId%s-B-ontime-$idx%03d", startTimestamp)
        withWorkerInShiftC(f"$factoryId%s-C-ontime-$idx%03d", startTimestamp)
      }

      // Worker that is late - no events
    }
  }


  class ShiftTeam(shift: Shift) extends Ensemble {
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

      allow(shift.foreman, "enter", shift.workPlace)
      allow(assignedWorkers, "enter", shift.workPlace)
    }


    object NotificationOfWorkersThatArePotentiallyLate extends Ensemble {
      name(s"NotificationOfWorkersThatArePotentiallyLate")

      val workersThatAreLate = assignedWorkers.filter(wrk => !(wrk isAt shift.workPlace.factory))

      situation {
        now isAfter (shift.startTime minusMinutes 20)
      }

      notify(workersThatAreLate, HurryUpNotification(shift))
      allow(shift.foreman, "read.personalData.phoneNo", workersThatAreLate)
      allow(shift.foreman, "read.distanceToWorkPlace", workersThatAreLate)
    }


    object CancellationOfWorkersThatAreLate extends Ensemble {
      name(s"CancellationOfWorkersThatAreLate")

      val workersThatAreLate = assignedWorkers.filter(wrk => !(wrk isAt shift.workPlace.factory))

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

      val selectedStandbys = unionOf(standbyAssignments.map(_.standby))

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

    object NoAccessToPersonalDataExceptForLateWorkers extends Ensemble {
        name(s"NoAccessToPersonalDataExceptForLateWorkers")

        val workersPotentiallyLate =
            if ((now isAfter (shift.startTime minusMinutes 20)) && (now isBefore shift.startTime))
                assignedWorkers.filter(wrk => !(wrk isAt shift.workPlace.factory))
            else
                Nil

        val workers = shift.workers diff workersPotentiallyLate

        deny(shift.foreman, "read.personalData", workers, PrivacyLevel.ANY)
        deny(shift.foreman, "read.personalData", workersPotentiallyLate, PrivacyLevel.SENSITIVE)
    }

    utility {
      AssignmentOfStandbys.utility
    }

    rules(
      // Grants
      AccessToTheHall,
      NotificationOfWorkersThatArePotentiallyLate,
      CancellationOfWorkersThatAreLate,
      AssignmentOfStandbys,
      AccessToTheDispenser,

      // Assertions
      NoAccessToPersonalDataExceptForLateWorkers
    )
  }

  class ShiftTeams extends RootEnsemble {
    name(s"Shift teams")

    val shiftTeams = rules(shiftsMap.values.map(shift => new ShiftTeam(shift)))

    constraints {
      shiftTeams.map(_.AssignmentOfStandbys.selectedStandbys).allDisjoint
    }

    utility {
      shiftTeams.sum(_.utility)
    }
  }

  val shiftTeams = root(new ShiftTeams)
}


object TestScenario {
  println("Saving log to test-scenario.log")
  val logPrintWriter = new PrintWriter(new File("test-scenario.log"))

  def log(): Unit = {
    logPrintWriter.println()
  }

  def log(msg: Any): Unit = {
    logPrintWriter.println(msg)
  }

  def main(args: Array[String]): Unit = {
    val scenario = new TestScenario
    scenario.init()

    val shiftTeams = scenario.shiftTeams

    log(scenario.workersMap)
    log(scenario.factoriesMap)
    log(scenario.shiftsMap)
    log(scenario.events)

    var tsSteps = scenario.events.map(_.timestamp).toSet.toList.sortWith((ts1, ts2) => ts1 isBefore ts2)


    val measurementsTs = LocalDateTime.parse("2018-12-03T08:55:00")
    val measurementsCount = 1000

    if (measurementsTs != null) {
      tsSteps = tsSteps.filter(_ isBefore measurementsTs)
    }


    // for (ts <- tsSteps) {
    {
      //scenario.now = ts
      scenario.now = measurementsTs
      println("Time: " + scenario.now)

      //val events = scenario.events filter(_.timestamp == scenario.now)
      val events = scenario.events filter(_.timestamp isBefore scenario.now)

      log()
      log("Time: " + scenario.now)
      log("Events: " + events)

      for (event <- events) {
        scenario.workersMap(event.person).position = event.position
      }

      shiftTeams.init()

      while (shiftTeams.solve()) {
      }

      if (shiftTeams.exists) {
        log("Utility: " + shiftTeams.instance.solutionUtility)
        log(shiftTeams.instance.toString)

        shiftTeams.commit()

        for (action <- shiftTeams.actions) {
          log(action)
        }

      } else {
        log("Error. No solution exists.")
      }
    }

    if (measurementsTs != null) {
      var perfAggTime = 0L

      for (measurementIdx <- 0 until measurementsCount) {
        val perfStartTime = System.currentTimeMillis()

        shiftTeams.init()
        while (shiftTeams.solve()) {
          print(".")
        }
        println()

        val perfEndTime = System.currentTimeMillis()
        println("Computation time (exists " + shiftTeams.exists + "): " + (perfEndTime - perfStartTime) / 1000.0 + " seconds")
        Console.out.flush()

        perfAggTime += perfEndTime - perfStartTime
      }

      println("Average time: " + perfAggTime / 1000.0 / measurementsCount + " seconds")
    }

  }

}


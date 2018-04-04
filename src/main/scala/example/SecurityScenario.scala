package example

import tcof._

import scala.collection.mutable

class SecurityScenario extends Model with VisualLogger {

  val random = scala.util.Random
  random.setSeed(42)

  var doors: List[Door] = _

  //
  // components
  //

  abstract class Room(name: String, val capacity: Int) extends Component {
    name(name)
  }

  class WorkingPlace(name: String, capacity: Int) extends Room(name, capacity)
  class LunchRoom(name: String, capacity: Int) extends Room(name, capacity)
  class Corridor(name: String) extends Room(name, Int.MaxValue)

  // serves as an one-way edge between room-nodes
  class Door(val srcRoom: Room, val tgtRoom: Room) {
    def enter(person: Person): Boolean = {
      val personToEnterTheRoom = new PersonToEnterTheRoom(person, tgtRoom)
      personToEnterTheRoom.isAllowedToEnter
    }
  }

  object Team extends Enumeration {
    val TeamA, TeamB = Value
  }

  object PersonMode extends Enumeration {
    val Work, Eat = Value  // + Sleep, but it is basically the same as the other modes
  }

  class Person(name: String, val team: Team.Value, var position: Room, val doors: Iterable[Door]) extends Component {

    name(name)
    var mode: PersonMode.Value = PersonMode.Work
    var targetRoom: Option[Room] = None

    def think(): Unit = {
      if (changeCurrentState()) {
        mode = if (mode == PersonMode.Eat) PersonMode.Work else PersonMode.Eat
      }
    }

    private def changeCurrentState(): Boolean = {
      // change the current state with probability 1/3
      // TODO - use Gaussian distribution to stay in the state for some number of steps?

      random.nextInt(9) == 0
    }

    def act(): Unit = {
      targetRoom match {
        case Some(tgt) =>
          nextDoor(position, tgt) match {
            case Some(selectedDoor) => {
              println(s"$name trying to move from ${selectedDoor.srcRoom} to ${selectedDoor.tgtRoom}")

              // log selected person before entering the door
              log(selected = List(this), Map.empty, Map.empty)

              if (selectedDoor.enter(this)) {
                position = selectedDoor.tgtRoom
                println(s"$name moved from ${selectedDoor.srcRoom} to ${selectedDoor.tgtRoom}")
                log(selected = List.empty, enteringPassing = Map(this -> selectedDoor), enteringRejected = Map.empty)
              } else {
                println(s"$name unable to move from ${selectedDoor.srcRoom} to ${selectedDoor.tgtRoom}")
                log(selected = List.empty, enteringPassing = Map.empty, enteringRejected = Map(this -> selectedDoor))
              }

              // log selected person after entering the door
              log(selected = List(this), Map.empty, Map.empty)
            }
            case None => // no path from srcRoom to tgtRoom. Also a case when person is already in tgtRoom
          }
        case None =>  // TODO - if no room is assigned, the person stays in the current room. Assign some room every time (e.g. corridor or exterier to move out) ?
      }
    }
  }


  //
  // rules checked by door when person is about to enter the room
  //

  var avoidTeamRules: List[AvoidTeamRule] = _

  // Class representing the rule - in the given room a person with the role
  // "personRole" cannot meet a person with the role "toAvoid".
  case class AvoidTeamRule(room: Room, personTeam: Team.Value, toAvoid: Team.Value)

  class PersonToEnterTheRoom(person: Person, room: Room) {
    def isAllowedToEnter: Boolean = {
      val teamsInRoom = components.collect{ case p: Person => p }
                                  .filter(_.position == room)
                                  .map(_.team).toSet
      avoidTeamRules.forall {
        case AvoidTeamRule(`room`, person.team, teamToAvoid) =>
          !teamsInRoom.contains(teamToAvoid)
        case _ => true
      }
    }
  }

  //
  // ensembles
  //

  class AssignRooms(val room: Room, val team: Team.Value, val personMode: PersonMode.Value) extends Ensemble {
    name(s"Persons for room $room")

    // components are filtered first by values that do not change (team)
    val persons = role(s"Persons for room $room", components.select[Person].filter(p => p.team == team))

    membership {
      // - all the persons must be in mode personMode
      // - number of persons in room must not exceed room capacity
      persons.all(_.mode == personMode) && persons.cardinality <= room.capacity
    }

    def timeToReachRoom(persons: Role[Person]) =
      persons.sum(p => dist(p.position, room))

    utility {
      // TODO - utility with timeToReachRoom works but takes too long to compute
      //(persons.cardinality * 100) - timeToReachRoom(persons)
      persons.cardinality
    }
  }

  class System extends RootEnsemble {

    val teamAWorkingRooms = ensembles("Team A working rooms",
      components.select[WorkingPlace]
                .map(new AssignRooms(_, Team.TeamA, PersonMode.Work))
    )
    val teamBWorkingRooms = ensembles("Team B working rooms", components.select[WorkingPlace].map(new AssignRooms(_, Team.TeamB, PersonMode.Work)))

    val teamALunchRooms = ensembles("Team A lunch rooms", components.select[LunchRoom].map(new AssignRooms(_, Team.TeamA, PersonMode.Eat)))
    val teamBLunchRooms = ensembles("Team B lunch rooms", components.select[LunchRoom].map(new AssignRooms(_, Team.TeamB, PersonMode.Eat)))

    membership {
      // - every person can be assigned only to one room
      // - room can be assigned to at most one team

      // NOTE: this doesn't behave as expected - probably cannot concat iterables, must create ensemble
//      (teamAWorkingRooms.map(_.persons) ++ teamBWorkingRooms.map(_.persons)).allDisjoint &&
      teamAWorkingRooms.map(_.persons).allDisjoint && teamBWorkingRooms.map(_.persons).allDisjoint &&
      teamAWorkingRooms.disjointAfterMap(_.room, teamBWorkingRooms, (x: AssignRooms) => x.room) &&
      teamALunchRooms.map(_.persons).allDisjoint && teamBLunchRooms.map(_.persons).allDisjoint &&
      teamALunchRooms.disjointAfterMap(_.room, teamBLunchRooms, (x: AssignRooms) => x.room)
    }
  }

  val rootEnsemble: RootEnsembleAnchor[System] = root(new System)


  private val dist = mutable.Map[(Room, Room), Int]()
  private val next = mutable.Map[(Room, Room), Door]()

  // Uses Floyd-Warshall algorithm with path reconstruction
  def buildMap(): Unit = {
    val rooms = components.collect{case r: Room => r}
    //val doors = components.collect{case d: Door => d}

    for (r1 <- rooms; r2 <- rooms) {
      val initDist = if (r1 == r2) 0 else 10000
      dist += (r1, r2) -> initDist
    }

    for (door <- doors) {
      dist += (door.srcRoom, door.tgtRoom) -> 1
      next += (door.srcRoom, door.tgtRoom) -> door
    }

    for (k <- rooms; i <- rooms; j<- rooms) {
      if (dist(i, j) > dist(i, k) + dist(k, j)) {
        dist += (i, j) -> (dist(i, k) + dist(k, j))
        next += (i, j) -> next(i, k)
      }
    }
  }

  def nextDoor(from: Room, to: Room): Option[Door] = {
    next.get(from, to)
  }

  def propagateAssignedRoomsToComponents() = {
    val persons = components.collect{case p: Person => p}
    persons.foreach(_.targetRoom = None)

    val groups = List(
      rootEnsemble.instance.teamAWorkingRooms,
      rootEnsemble.instance.teamALunchRooms,
      rootEnsemble.instance.teamBWorkingRooms,
      rootEnsemble.instance.teamBLunchRooms
    )

    for (group <- groups; assignedRoom <- group.selectedMembers) {
      assignedRoom.persons.selectedMembers.foreach(_.targetRoom = Some(assignedRoom.room))
    }
  }
}

object SecurityScenario {
  def main(args: Array[String]): Unit = {
    val scenario = new SecurityScenario
    scenario.init()

    // Building has following map
    //
    //    +----+----+----+
    //    | L1 | L2 | L3 |
    //    +- --+- --+- --+
    //    | C1   C2   C3 |
    //    +- --+- --+- --+
    //    | W1 | W2 | W3 |
    //    +----+----+----+
    //
    // Rooms marked as W* are working places with capacities 2 (W1), 2 (W2) and 2 (W3).
    // Rooms marked as L* are lunch places with capacities 2 (L1), 2 (L2) and 2 (L3).
    // Rooms C* are corridors without capacity.
    // There are two teams A and B each with 5 members, goal is to schedule them
    // into working and lunch places, people from one team should avoid people from
    // the other one.

    val lunchRooms = List((1, 2), (2, 2), (3, 2)).map{case (i, capacity) => new scenario.LunchRoom(s"L$i", capacity)}
    val workingRooms = List((1, 2), (2, 2), (3, 2)).map{case (i, capacity) => new scenario.WorkingPlace(s"W$i", capacity)}
    val corridors = List(1, 2, 3).map(i => new scenario.Corridor(s"C$i"))

    val rooms = lunchRooms ++ corridors ++ workingRooms

    val doors = List(
      (lunchRooms(0), corridors(0)), (lunchRooms(1), corridors(1)), (lunchRooms(2), corridors(2)),
      (workingRooms(0), corridors(0)), (workingRooms(1), corridors(1)), (workingRooms(2), corridors(2)),
      (corridors(0), corridors(1)), (corridors(1), corridors(2))
    ).flatMap{case (r1, r2) => List(new scenario.Door(r1, r2), new scenario.Door(r2, r1))}

    val teamA = (1 to 5).map(i => new scenario.Person(s"Person A$i", scenario.Team.TeamA, corridors(0), doors))
    val teamB = (1 to 5).map(i => new scenario.Person(s"Person B$i", scenario.Team.TeamB, corridors(0), doors))

    val persons = teamA ++ teamB

    scenario.components = rooms ++ persons
    scenario.doors = doors

    scenario.avoidTeamRules = for {
      room <- (lunchRooms ++ workingRooms)
      personTeam <- scenario.Team.values
      toAvoid <- scenario.Team.values
      if personTeam != toAvoid
    } yield scenario.AvoidTeamRule(room, personTeam, toAvoid)

    scenario.buildMap()

    println("System initialized")

    // Simulation - each person decides to either stay in the room or move to another room.
    for (t <- 0 until 10) {
      println(s"--------------------------------------------------------")
      println(s"Step $t")
      scenario.step(t)

      persons.foreach(_.think())

      scenario.rootEnsemble.init()
      while (scenario.rootEnsemble.solve()) {
        //println(scenario.rootEnsemble.instance.toStringWithUtility)
      }
      println(scenario.rootEnsemble.instance.toStringWithUtility)
      scenario.rootEnsemble.commit()
      println(scenario.rootEnsemble.instance.solutionUtility)

      scenario.log(List.empty, Map.empty, Map.empty)

      // propagate assigned room from ensemble to persons
      scenario.propagateAssignedRoomsToComponents()

      println(persons.map(p => s"${p.name}: ${p.mode} ${p.position} -> ${p.targetRoom}").mkString("\n"))
      persons.foreach(_.act())
    }
  }

}

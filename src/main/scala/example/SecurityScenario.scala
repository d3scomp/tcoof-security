package security

import tcof._

class SecurityScenario extends Model {

  val random = scala.util.Random

  //
  // components
  //

  abstract class Room(name: String, val capacity: Int) extends Component {
    name(name)
  }

  class WorkingPlace(name: String, capacity: Int) extends Room(name, capacity)
  class LunchRoom(name: String, capacity: Int) extends Room(name, capacity)
  class Corridor(name: String) extends Room(name, Int.MaxValue)
  object Exterior extends Room("Exterior", Int.MaxValue)

  // serves as an one-way edge between room-nodes
  class Door(val srcRoom: Room, val tgtRoom: Room) extends Component {
    def enter(person: Person): Boolean = {

      /*
      // check with ensembles whether the person can enter or not

      val rootEnsemble = root(new PersonToEnterTheRoom(person, tgtRoom))
      rootEnsemble.init()
      rootEnsemble.solve()
      while (rootEnsemble.solve()) {
        println(rootEnsemble.instance.toString)
      }
      rootEnsemble.commit()
      println(rootEnsemble.instance.solutionUtility)
*/
      true
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

    def act(): Unit = {
//      val doorsFromRoom = doors.filter(_.srcRoom == position)
//
//      // door to open is selected randomly
//      val selectedDoor = doorsFromRoom.toList(random.nextInt(doorsFromRoom.size))
//
//      println(s"$name trying to move from ${selectedDoor.srcRoom} to ${selectedDoor.tgtRoom}")
//      if (selectedDoor.enter(this)) {
//        position = selectedDoor.tgtRoom
//        println(s"$name moved from ${selectedDoor.srcRoom} to ${selectedDoor.tgtRoom}")
//      } else {
//        println(s"$name unable to move from ${selectedDoor.srcRoom} to ${selectedDoor.tgtRoom}")
//      }
    }
  }

  //
  // ensembles
  //

  /*
  // In the given room a person with the role "personRole" cannot meet a person with the role "toAvoid".
  // NOTE: The relation is not symmetric.
  class AvoidRoles(room: Room, personRole: Team.Value, toAvoid: Team.Value) extends Ensemble {
    val persons = role("allPersons", components.select[Person])

//    membership(
//      //persons.all()
//    )
  }

  // An ensemble between the given room and all persons within that room.
  class PersonsInRoom(room: Room) extends Ensemble {
    val persons = role("allPersonsInRoom",
      components.collect{ case p: Person => p }.filter(_.position == room))

    // alternative - use membership function
//    val persons = role("allPersons", components.select[Person])
//    membership(
//      persons.all(_.position == room)
//    )
  }

  class PersonToEnterTheRoom(person: Person, room: Room) extends RootEnsemble {
    val personsInRoom = new PersonsInRoom(room)

    // all avoidance rules which apply to given room and roles of the given person
//    val toAvoid = ensembles("RolesInRoomToAvoid", avoidRoles.map(x => new AvoidRoles(x._1, x._2, x._3))
//          .filter(r => r.room == room)
//          .filter(r => person.roles.contains(r.personRole))
//    )

    val rolesInRoomToAvoid = avoidRoles.filter(x => x._1 == room)
                                       .filter(x => person.team == x._2)
                                       .map(_._3).toSet

//    val rolesInRoom = personsInRoom.persons.

    membership(
      // TODO - add emergency mode
      // check whether person with its role can enter the room - need access to AvoidRoles rules
      // which are set from outside

      // all the roles of the persons in room
//      personsInRoom.persons.allMembers.values.flatMap(_.roles).toSet
//      (personsInRoom.persons.values.flatMap(_.roles).toSet
//      intersect
//      toAvoid.map(_.allMembers).map(_.toAvoid))
//        .isEmpty

//      rolesInRoomToAvoid


      // roles to avoid
      //toAvoid.allMembers.map(_.toAvoid)



      true
    )
  }

  // TODO - place somewhere else?
  var avoidRoles: List[(Room, Team.Value, Team.Value)] = _
*/
  class AssignRooms(val room: Room, team: Team.Value, personMode: PersonMode.Value) extends Ensemble {
    name(s"Persons for room $room")

    // components are filtered first by values that do not change (team)
    val persons = role(s"Persons for room $room", components.select[Person].filter(p => p.team == team))
    // TODO - not ellegant
    //val assignedRoom = role(s"Assigned room $room", components.select[Room].filter(r => r == room))

    membership {
      // - all the persons must be in mode personMode
      // - number of persons in room must not exceed room capacity
      persons.all(_.mode == personMode) && persons.cardinality <= room.capacity
    }

    utility {
      persons.cardinality
    }
  }

  class System extends RootEnsemble {

    val teamAWorkingRooms = ensembles("Team A working rooms", components.select[WorkingPlace].map(new AssignRooms(_, Team.TeamA, PersonMode.Work)))
    val teamBWorkingRooms = ensembles("Team B working rooms", components.select[WorkingPlace].map(new AssignRooms(_, Team.TeamB, PersonMode.Work)))

//    val teamALunchRooms = ensembles("Team A lunch rooms", components.select[LunchRoom].map(new AssignRooms(_, Team.TeamA, PersonMode.Eat)))
//    val teamBLunchRooms = ensembles("Team B lunch rooms", components.select[LunchRoom].map(new AssignRooms(_, Team.TeamB, PersonMode.Eat)))

    // TODO - add and use foreach instead of map
    teamAWorkingRooms.map(teamA => teamA.membership{ !teamBWorkingRooms.selectedMembers.exists(_.room == teamA.room) })

    membership {
      // - every person can be assigned only to one room
      // - room can be assigned to at most one team

      // TODO: this doesn't behave as expected - probably cannot concat iterables, must create ensemble
      //(teamAWorkingRooms.map(_.persons) ++ teamBWorkingRooms.map(_.persons)).allDisjoint

      teamAWorkingRooms.map(_.persons).allDisjoint && teamBWorkingRooms.map(_.persons).allDisjoint //&&
      //(teamAWorkingRooms.map(_.assignedRoom) ++ teamBWorkingRooms.map(_.assignedRoom)).allDisjoint
        //teamAWorkingRooms.map(_.assignedRoom).allDisjoint(teamBWorkingRooms.map(_.assignedRoom))


      //&& (teamAWorkingRooms.map(_.assignedRoom) ++ teamBWorkingRooms.map(_.assignedRoom)).allDisjoint


      //true
    }
  }

  val rootEnsemble: RootEnsembleAnchor[System] = root(new System)
}

object SecurityScenario {
  def main(args: Array[String]): Unit = {
    val scenario = new SecurityScenario
    scenario.init()

    // Building has following map, rooms X (exterior) just simulates space outside of the building.
    //
    //  +----------------------+
    //  |                      |
    //  |   +----+----+----+   |
    //  | X | L1 | L2 | L3 |   |
    //  |   +- --+- --+- --+   |
    //  |     C1   C2   C3 |   |
    //  |   +- --+- --+- --+   |
    //  |   | W1 | W2 | W3 |   |
    //  |   +----+----+----+   |
    //  |                      |
    //  +----------------------+
    //
    // Rooms marked as W* are working places with capacities 2 (W1), 3 (W2) and 4 (W3).
    // Rooms marked as L* are lunch places with capacities 3 (L1), 3 (L2) and 3 (L3).
    // Rooms C* are corridors without capacity.
    // There are two teams A and B each with 4 members, goal is to schedule them
    // into working and lunch places, people from one team should avoid people from
    // the other one.

    //val lunchRooms = List((1, 3), (2, 3), (3, 3)).map{case (i, capacity) => new scenario.LunchRoom(s"L$i", capacity)}
    val lunchRooms: List[scenario.LunchRoom] = List()
    val workingRooms = List((1, 3), (2, 3), (3, 3)).map{case (i, capacity) => new scenario.WorkingPlace(s"W$i", capacity)}
    //val corridors = (1 to 3).map(i => new scenario.Corridor(s"C$i"))

    // Note that exterior is not part of the building
    val rooms = lunchRooms ++ workingRooms /* ++ corridors */

//    val doors = List(
//      (lunchRooms(0), corridors(0)), (lunchRooms(1), corridors(1)), (lunchRooms(2), corridors(2)),
//      (workingRooms(0), corridors(0)), (workingRooms(1), corridors(1)), (workingRooms(2), corridors(2)),
//      (corridors(0), corridors(1)), (corridors(1), corridors(2)),
//      (scenario.Exterior, corridors(0))
//    ).flatMap{case (r1, r2) => List(new scenario.Door(r1, r2), new scenario.Door(r2, r1))}

    val doors: List[scenario.Door] = List()

    val teamA = (1 to 10).map(i => new scenario.Person(s"Person A$i", scenario.Team.TeamA, scenario.Exterior, doors))
    val teamB = (1 to 5).map(i => new scenario.Person(s"Person B$i", scenario.Team.TeamB, scenario.Exterior, doors))

    val persons = teamA ++ teamB

    scenario.components = rooms ++ doors ++ persons

    // avoid meeting of TeamA and TeamB in lunch rooms and working rooms
//    scenario.avoidRoles = (lunchRooms ++ workingRooms).flatMap(r =>
//        List((r, scenario.Team.TeamA, scenario.Team.TeamB), (r, scenario.Team.TeamB, scenario.Team.TeamA))
//    )

    scenario.rootEnsemble.init()
    println("System initialized")

    while (scenario.rootEnsemble.solve()) {
      println(scenario.rootEnsemble.instance.toStringWithUtility)
    }

    scenario.rootEnsemble.commit()

    println(scenario.rootEnsemble.instance.solutionUtility)

//    scenario.rootEnsemble.instance.toStringWithUtility

    // Simulation - each person decides to either stay in the room or move to another room.
//    for (t <- 0 until 10) {
//      println(s"Step $t")
//      persons.foreach(_.act())
//    }
  }

}

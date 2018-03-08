package security

import tcof.{Component, Ensemble, Model, RootEnsemble}

class SecurityScenario extends Model {

  //
  // components
  //

  class Building(val rooms: Set[Room], val doors: Set[Door]) extends Component

  class Room(name: String, val capacity: Int) extends Component {
    name(name)
  }

  // serves as an one-way edge between room-nodes
  class Door(val srcRoom: Room, val tgtRoom: Room) extends Component {
    def enter(person: Person): Boolean = {

      // check with ensembles whether the person can enter or not

      val rootEnsemble = root(new PersonToEnterTheRoom(person, tgtRoom))
      rootEnsemble.init()
      rootEnsemble.solve()
      while (rootEnsemble.solve()) {
        println(rootEnsemble.instance.toString)
      }
      rootEnsemble.commit()
      println(rootEnsemble.instance.solutionUtility)

      true
    }
  }

  object Role extends Enumeration {
    val TeamA, TeamB = Value
  }

  object PersonState extends Enumeration {
    val HeadingToLunch, HeadingToWorkplace, Leaving, Idle = Value
  }

  class Person(name: String, val roles: List[Role.Value], var position: Room, val doors: Set[Door]) extends Component {
    name(name)
    var mode: PersonState.Value = PersonState.Idle

    def act(): Unit = {
      val doorsFromRoom = doors.filter(_.srcRoom == position)
      val selectedDoor = doorsFromRoom.toList(0)

      println(s"$name trying to move from ${selectedDoor.srcRoom} to ${selectedDoor.tgtRoom}")
      if (selectedDoor.enter(this)) {
        position = selectedDoor.tgtRoom
        println(s"$name moved from ${selectedDoor.srcRoom} to ${selectedDoor.tgtRoom}")
      } else {
        println(s"$name unable to move from ${selectedDoor.srcRoom} to ${selectedDoor.tgtRoom}")
      }
    }
  }

  //
  // ensembles
  //

  // All doors in the building should allow pass through for any person.
  // References external variable "emergency" which depicts whether emergency mode is active or not.
//  class EmergencyModeDoors(val building: Building, emergency: Boolean) extends Ensemble {
//    name(s"DoorsInEmergencyMode $emergency for building $building")
//
//    val allDoors = role("allDoors", components.select[Door])
//
//    membership(
//      emergency == true
//    )
//  }

  // In the given room a person with the role "personRole" cannot meet a person with the role "toAvoid".
  // NOTE: The relation is not symmetric.
  class AvoidRoles(val room: Room, val personRole: Role.Value, val toAvoid: Role.Value) extends Ensemble {
    val persons = role("allPersons", components.select[Person])

//    membership(
//      //persons.all()
//    )
  }

  // An ensemble between the given room and all persons within that room.
  class PersonsInRoom(val room: Room) extends Ensemble {
    val persons = role("allPersonsInRoom",
      components.collect{ case p: Person => p }.filter(_.position == room))

    // alternative - use membership function
//    val persons = role("allPersons", components.select[Person])
//    membership(
//      persons.all(_.position == room)
//    )
  }

  class PersonToEnterTheRoom(val person: Person, val room: Room) extends RootEnsemble {
    val personsInRoom = new PersonsInRoom(room)

    // all avoidance rules which apply to given room and roles of the given person
//    val toAvoid = ensembles("RolesInRoomToAvoid", avoidRoles.map(x => new AvoidRoles(x._1, x._2, x._3))
//          .filter(r => r.room == room)
//          .filter(r => person.roles.contains(r.personRole))
//    )

    val rolesInRoomToAvoid = avoidRoles.filter(x => x._1 == room)
                                       .filter(x => person.roles.contains(x._2))
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
  var avoidRoles: List[(Room, Role.Value, Role.Value)] = _

  // Person must be able to leave the building.


}

object TestScenario {
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
    //  +-------------------- -+
    //
    // Rooms marked as W* are working places with capacities 2 (W1), 3 (W2) and 4 (W3).
    // Rooms marked as L* are lunch places with capacities 3 (L1), 3 (L2) and 3 (L3).
    // Rooms C* are corridors without capacity.
    // There are two teams A and B each with 4 members, goal is to schedule them
    // into working and lunch places, people from one team should avoid people from
    // the other one.
    // Modes of the person: HEADING_TO_LUNCH, HEADING_TO_WORKPLACE, LEAVING, IDLE

    val lunchRooms = List((1, 3), (2, 3), (3, 3)).map{case (i, capacity) => new scenario.Room(s"L$i", capacity)}
    val workingRooms = List((1, 2), (2, 3), (3, 4)).map{case (i, capacity) => new scenario.Room(s"W$i", capacity)}
    val corridors = (1 to 3).map(i => new scenario.Room(s"C$i", Int.MaxValue))
    val exterior = new scenario.Room("Exterior", Int.MaxValue)

    // Note that exterior is not part of the building
    val rooms = (lunchRooms ++ workingRooms ++ corridors).toSet

    val doors = List(
      (lunchRooms(0), corridors(0)), (lunchRooms(1), corridors(1)), (lunchRooms(2), corridors(2)),
      (workingRooms(0), corridors(0)), (workingRooms(1), corridors(1)), (workingRooms(2), corridors(2)),
      (corridors(0), corridors(1)), (corridors(1), corridors(2)),
      (exterior, corridors(0))
    ).flatMap{case (r1, r2) => List(new scenario.Door(r1, r2), new scenario.Door(r2, r1))}.toSet

    val building = new scenario.Building(rooms, doors)

    val teamA = (1 to 4).map(i => new scenario.Person(s"Person A$i", List(scenario.Role.TeamA), exterior, doors))
    val teamB = (1 to 4).map(i => new scenario.Person(s"Person B$i", List(scenario.Role.TeamB), exterior, doors))

    val persons = teamA ++ teamB

    scenario.components = List(building) ++ persons

    scenario.avoidRoles = rooms.flatMap(r =>
        List((r, scenario.Role.TeamA, scenario.Role.TeamB), (r, scenario.Role.TeamB, scenario.Role.TeamA))
    ).toList

    // Simulation - each person decides to either stay in the room or move to another room.
    for (t <- 0 until 10) {
      println(s"Step $t")
      persons.foreach(_.act())
    }
  }

}

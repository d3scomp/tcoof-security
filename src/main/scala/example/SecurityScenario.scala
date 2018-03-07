package security

import tcof.{Component, Ensemble, Model, RootEnsemble}

class SecurityScenario extends Model {

  //
  // components
  //

  class Building(val rooms: Set[Room], val doors: Set[Door]) extends Component

  class Room(name: String) extends Component {
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

  class Person(name: String, val roles: List[Role.Value], var position: Room, val doors: Set[Door]) extends Component {
    name(name)

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
    val toAvoid = avoidRoles.filter(_._1 == room).map(x => new AvoidRoles(x._1, x._2, x._3))

    membership(
      // TODO - add emergency mode
      // check whether person with its role can enter the room - need access to AvoidRoles rules
      // which are set from outside
      ???
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
    //  +---------------+
    //  |               |
    //  |   +---+---+   |
    //  | X   A   B |   |
    //  |   +- -+- -+   |
    //  |   | C   D     |
    //  |   +---+---+   |
    //  |               |
    //  +---------------+
    //
    // Room A at the beginning contains 2 persons from TeamA
    // Room D at the beginning contains 2 persons from TeamB

    val roomA = new scenario.Room("A")
    val roomB = new scenario.Room("B")
    val roomC = new scenario.Room("C")
    val roomD = new scenario.Room("D")
    val exterior = new scenario.Room("Exterior")

    // Note that roomX is not part of the building
    val rooms = Set(roomA, roomB, roomC, roomD)

    val doors = Set(
      new scenario.Door(roomA, roomB), new scenario.Door(roomB, roomA),
      new scenario.Door(roomC, roomD), new scenario.Door(roomD, roomC),
      new scenario.Door(roomA, roomC), new scenario.Door(roomC, roomA),
      new scenario.Door(roomB, roomD), new scenario.Door(roomD, roomB),
      new scenario.Door(exterior, roomA), new scenario.Door(roomA, exterior),
      new scenario.Door(exterior, roomD), new scenario.Door(roomD, exterior)
    )

    val building = new scenario.Building(rooms, doors)

    val adam = new scenario.Person("Adam", List(scenario.Role.TeamA), roomA, doors)
    val alice = new scenario.Person("Alice", List(scenario.Role.TeamA), roomA, doors)
    val bob = new scenario.Person("Bob", List(scenario.Role.TeamB), roomD, doors)
    val brenda = new scenario.Person("Brenda", List(scenario.Role.TeamB), roomD, doors)

    val persons = List(adam, alice, bob, brenda)

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

package security

import tcof.{Component, Ensemble, Model}

class SecurityScenario extends Model {

  //
  // components
  //

  class Building(val rooms: Set[Room], val doors: Set[Door]) extends Component

  class Room extends Component

  // serves as an one-way edge between room-nodes
  class Door(val srcRoom: Room, val tgtRoom: Room) extends Component

  object Role extends Enumeration {
    val TeamA, TeamB = Value
  }

  class Person(name: String, val roles: List[Role.Value], val position: Room) extends Component {
    name(name)
  }

  //
  // ensembles
  //

  // All doors in the building should allow pass through for any person.
  // References external variable "emergency" which depicts whether emergency mode is active or not.
  class EmergencyModeDoors(val building: Building, emergency: Boolean) extends Ensemble {
    name(s"DoorsInEmergencyMode $emergency for building $building")

    val allDoors = role("allDoors", components.select[Door])

    membership(
      emergency == true
    )
  }

  // In the given building the two different persons with the given roles cannot meet in
  // the same room.
  // NOTE: relation is not symmetric
  class AvoidRoles(val building: Building, val personRole: Role.Value, val toAvoid: Role.Value) extends Ensemble {

  }
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

    val roomA = new scenario.Room()
    val roomB = new scenario.Room()
    val roomC = new scenario.Room()
    val roomD = new scenario.Room()
    val exterior = new scenario.Room()

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

    val adam = new scenario.Person("Adam", List(scenario.Role.TeamA), roomA)
    val alice = new scenario.Person("Alice", List(scenario.Role.TeamA), roomA)
    val bob = new scenario.Person("Bob", List(scenario.Role.TeamB), roomD)
    val brenda = new scenario.Person("Brenda", List(scenario.Role.TeamB), roomD)



  }

}

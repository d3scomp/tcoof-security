package security

import tcof.{Component, Ensemble, Model}

import scala.collection.mutable

class SecurityScenario extends Model {

  //
  // components
  //

  class Building(val rooms: Set[Room], val doors: Set[Door]) extends Component

  class Room(val persons: mutable.Set[Person]) extends Component

  // serves as an one-way edge between room-nodes
  class Door(val srcRoom: Room, val tgtRoom: Room) extends Component {
//    def pass(person: Person): Boolean = {
//      // TODO - create new instance of rooms?
//
//      false
//    }
  }

  class Person(val doorToOpen: Option[Door]) extends Component

  //
  // ensembles
  //

  // each person with doorToOpen set forms PersonEnteringDoor ensemble with the given door
  class PersonEnteringDoor(val door: Door, val person: Person) extends Ensemble {
    name(s"PersonEnteringDoor $person for door $door")
  }

  // all doors in the building should allow pass through for any person
  // references external variable "emergency" which depicts whether emergency mode is active or not
  class EmergencyModeDoors(val building: Building, emergency: Boolean) extends Ensemble {
    name(s"DoorsInEmergencyMode $emergency for building $building")

    val allDoors = role("allDoors", components.select[Door])

    membership(
      emergency == true
    )
  }

  // NOTE: SeparatingDoors relation is not symmetric
  class SeparatingDoors(val person: Person, val personToAvoid: Person) extends Ensemble {
    name(s"Person $person must avoid $personToAvoid")

    val doorsSeparatingPersons = role("doorsSeparatingPersons", components.select[Door])

    membership(
      doorsSeparatingPersons.all(door =>
        door.srcRoom.persons.contains(person) && door.tgtRoom.persons.contains(personToAvoid)
      )
    )
  }


}

object TestScenario {
  def main(args: Array[String]): Unit = {
    println("hello")
  }

  // ensemble, ktery modeluje vsechny osoby, ktere mohou projit dvermi?

}

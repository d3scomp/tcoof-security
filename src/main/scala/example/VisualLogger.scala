package example

import java.io.{File, PrintWriter}
import java.nio.charset.Charset
import java.nio.file.{Files, Path, Paths}
import java.time.Instant

trait VisualLogger {
  this: SecurityScenario =>

  // Called when:
  // - when all agents change their mode
  // - solver computes the utility and assigns target rooms
  // - each person tries to move <- need to mark selected person

  private var frameCnt = 0
  private val roomSize = 250
  private lazy val roomVizualizations = getRoomVizualizations()
  private lazy val doorPositions = getDoorToPositionMap()
  private val logDir = createTmpDir()

  copyCss()

  private def createTmpDir(): File = {
    val dateString = Instant.now.getEpochSecond.toString
    val dir = new File("visualization/" + dateString)
    dir.mkdirs()
    dir
  }

  private def copyCss(): Unit = {
    val src = Paths.get(getClass.getResource("/visualisation.css").toURI)
    val tgt = Paths.get(logDir.getPath, "visualisation.css")
    Files.copy(src, tgt)
  }

  private def getRoomVizualizations(): Seq[RoomVizualization] = {
    val rooms = lunchRooms ++ corridors ++ workingRooms

    val positions = for (top <- (0 to 500).by(roomSize); left <- (0 to 500).by(roomSize)) yield Position(top, left)
    rooms.zip(positions).map(x => new RoomVizualization(x._1, x._2))
  }

  private def getDoorToPositionMap() = {
    val positions = (for (top <- List(215, 465); left <-List(97, 347, 597)) yield Position(top, left)) ++ List(Position(337, 222), Position(337, 472))
    // each door is modeled as two one-way Door instances
    val doubledPositions = positions.flatMap(p => List(p, p))
    doors.zip(doubledPositions).toMap
  }

  def log(selected: List[Person], enteringPassing: Map[Person, Door], enteringRejected: Map[Person, Door]): Unit = {
    frameCnt += 1

    val file = new File(logDir.getPath + "/" + f"${frameCnt}%06d" + ".html")
    val writer = new PrintWriter(file)

    val personsToNotPrintInRooms = (enteringPassing ++ enteringRejected).keys.toSeq

    try {
      writer.println(header)

      for (r <- roomVizualizations) {
        writer.println(r.print(personsToNotPrintInRooms, selected))
      }

      writer.println(doorsString)

      for ((person, door) <- enteringPassing) {
        val vis = new PersonVizualization(person)
        val pos = doorPositions(door)
        writer.println(vis.print(Some(pos), HighLighting.Passing))
      }

      for ((person, door) <- enteringRejected) {
        val vis = new PersonVizualization(person)
        val pos = doorPositions(door)
        writer.println(vis.print(Some(pos), HighLighting.Rejected))
      }

      writer.println(stepAndUtility)

      writer.println(footer)
    } finally {
      writer.close()
    }
  }

  def stepAndUtility =
    s"""<div id="info">step: ${this.time} utility: ${rootEnsemble.instance.solutionUtility}</div>"""

  val header =
    s"""
       |<!DOCTYPE html>
       |<html>
       |<head>
       |    <link rel="stylesheet" type="text/css" href="visualisation.css">
       |</head>
       |<body>
     """.stripMargin

  val footer =
    s"""
       |</body>
       |</html>
     """.stripMargin

  val doorsString = s"""
                 |<!-- doors -->
                 |<span class="door-vert" style="position:absolute; top:340px; left: 247px"></span>
                 |<span class="door-vert" style="position:absolute; top:340px; left: 497px"></span>
                 |
                 |<span class="door-horiz" style="position:absolute; top:247px; left: 90px"></span>
                 |<span class="door-horiz" style="position:absolute; top:247px; left: 340px"></span>
                 |<span class="door-horiz" style="position:absolute; top:247px; left: 590px"></span>
                 |
                 |<span class="door-horiz" style="position:absolute; top:497px; left: 90px"></span>
                 |<span class="door-horiz" style="position:absolute; top:497px; left: 340px"></span>
                 |<span class="door-horiz" style="position:absolute; top:497px; left: 590px"></span>
               """.stripMargin

  class RoomVizualization(val room: Room, val position: Position) {
    val classStyle = room match {
      case _: WorkingPlace => "working-room"
      case _: LunchRoom => "lunch-room"
      case _ => ""
    }

    def print(personsToNotPrint: Seq[Person], personsToPrintSelected: Seq[Person]): String = {
      val personStrings = components.collect{ case p: Person => p }
        .filter(_.position == room)
        .filterNot(personsToNotPrint.contains(_))
        .map {p =>
          if (personsToPrintSelected.contains(p))
            new PersonVizualization(p).print(None, HighLighting.Selected)
          else
            new PersonVizualization(p).print(None)
        }

      s"""
         |<span class="room ${classStyle}" style="position:absolute; top:${position.top}px; left: ${position.left}px">
         |  <span class="room-name">${room.name}</span>
         |  ${personStrings.mkString("\n")}
         |</span>
       """.stripMargin
    }
  }

  object HighLighting extends Enumeration {
    val None, Selected, Passing, Rejected = Value
  }

  class PersonVizualization(val person: Person) {

    def print(position: Option[Position], highLighting: HighLighting.Value = HighLighting.None): String = {
      val mode = if (person.mode == PersonMode.Eat) "eat" else "work"
      val team = if (person.team == Team.TeamA) "team-a" else "team-b"

      val highlightingString = highLighting match {
        case HighLighting.Selected => "selected"
        case HighLighting.Passing => "passing"
        case HighLighting.Rejected => "rejected"
        case _ => ""
      }

      val tgt = person.targetRoom match {
        case None => ""
        case Some(room) => " " + room.name
      }

      val label = person.name.stripPrefix("Person ") + tgt

      val styleString = position match {
        case Some(Position(top, left)) => s"""style="position:absolute; top:${top}px; left: ${left}px""""
        case None => ""
      }

      s"""<span class="person ${mode} ${team} ${highlightingString}" ${styleString}>${label}</span>"""
    }
  }

  case class Position(top: Int, left: Int)
}

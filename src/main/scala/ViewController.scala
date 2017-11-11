import java.net.ConnectException

import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Circle, Line, Rectangle}
import javafx.scene.shape.{Circle => JavaFxCircle, Line => JavaFxLine, Rectangle => JavaFxRectangle}

import PathFinder.{calculatePath, getStartingNode}

import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.scene.Node
import scalafx.scene.paint.Color

object ViewController {
  val CANVAS_WIDTH = 1024
  val CANVAS_HEIGHT = 1024
  val BUTTON_BAR_WIDTH = 200
  var LEVELS: List[Level] = List()
  var BOT_RUNNING = true
  var SELECTED_TARGET: Option[Vertex] = None
  val DEFAULT_WAD_LOCATION = "/home/neil/Downloads/doom1.wad"

  // Callbacks

  def loadWad(): Unit = {
    val wad = WadParser.createWad(getWadFile)
    LEVELS = wad.levels
    val levelNames = LEVELS.map(_.name)
    Platform.runLater {
      DoomViewer.mapComboBox.items = ObservableBuffer(levelNames)
      DoomViewer.mapComboBox.value = LEVELS.head.name // default to the first map
    }
  }

  def changeLevel(name: String): Unit = {
    val level = LEVELS.find(_.name == name)
    showLevel(level.getOrElse(LEVELS.head))
    BOT_RUNNING = false
    SELECTED_TARGET = None
    GameInterface.changeLevel(name(1), name(3))   // This fails if the game isn't running
  }

  def boundingBoxToggle(showBoxes: Boolean): Unit = {
    if (showBoxes){
      val lines = fitLinesToScreen(LineMXQuadTree.allExternalLines(getLevel.linedefs))
      val boxes = lines.map(lineToRect(_, Red))
      boxes.foreach{DoomViewer.mapPane.children.add(_)}
    } else {
      showLevel(getLevel)
    }
  }

  def quadTreeToggle(shouldShowQuadTree: Boolean): Unit =
    if (shouldShowQuadTree) showQuadTree(getLevel)
    else showLevel(getLevel)

  def paneClicked(x: Double, y: Double): Unit = {
    val wadPoint = screenPointToWadPoint(x, y)
    val level = LEVELS.find(_.name == DoomViewer.mapComboBox.value.value).get
    if (DoomViewer.showQuadTreeButton.selected.value) {
      val lines = level.quadTree.getLinesForPoint(wadPoint)
      if (lines.nonEmpty) {
        showLevel(level)
        showQuadTree(level)
        val screenLines = makeLinesForDisplay(lines, colour = Some(Red))
        screenLines.foreach {
          DoomViewer.mapPane.children.add(_)
        }
      }
    } else {
      SELECTED_TARGET = Some(wadPoint)
      showLevel(level)
      drawNode(SELECTED_TARGET.get, Blue)
      println(s"x: ${wadPoint.x}, y: ${wadPoint.y}")
    }
  }

  def generatePath(): Unit = GraphBuilder.genGraphForLevel(getLevel, drawPath = true)

  def drawPathLine(line: Linedef): Unit = {
    val pathLine = makeLinesForDisplay(List(line), colour = Some(Green)).head
    Platform.runLater {
      DoomViewer.mapPane.children.add(pathLine)
    }
  }

  def drawNode(location: Vertex, colour: scalafx.scene.paint.Color = Red): Unit = {
    val screenLocation = wadPointToScreenPoint(location)
    Platform.runLater {
      val circle = new Circle(new JavaFxCircle(screenLocation.x, screenLocation.y, 3))
      circle.fill = colour
      DoomViewer.mapPane.children.add(circle)
    }
  }

  def clearScreen(): Unit = showLevel(getLevel)

  def startBot(): Unit = {
    PlayerController.startBot()
  }

  def drawWorldObjects(): Unit = {
    //Doors from the game engine
    val doors = GameInterface.getAllDoors
    val doorLines = doors.map(door => Linedef(door.line.v1, door.line.v2, nonTraversable = false))
    val screenDoorLines = makeLinesForDisplay(doorLines, colour = Some(Red))
    //Platform.runLater {
    //  screenDoorLines.foreach { DoomViewer.mapPane.children.add(_) }
    //}

    //Doors from the WAD
    val doorSwitches = getLevel.doorSwitches
    val doorLinedefs = doorSwitches.map(switch => Linedef(switch.a, switch.b, nonTraversable = false))
    val screenDoorLinedefs = makeLinesForDisplay(doorLinedefs, colour = Some(HotPink), width = Some(3))
    Platform.runLater {
      screenDoorLinedefs.foreach { DoomViewer.mapPane.children.add(_) }
    }
  }

  def findPathCallback(level: Level): Unit = {
    val startingNode = getStartingNode(level)
    val targetNode = new PathNode(ViewController.SELECTED_TARGET.getOrElse(level.exit), startingNode.getLevel)
    ViewController.drawNode(targetNode.getLocation, Blue)
    val path = calculatePath(startingNode, targetNode, List(), drawPathOnly = true)
    path match {
      case None => println("No path to target")
      case _ =>
    }
  }


  // Private methods

  def getWadFile: String = {
    try {
      GameInterface.getWorldSettings.wad
    } catch {
      case e: ConnectException => DEFAULT_WAD_LOCATION
    }
  }

  def getLevel: Level = {
    LEVELS.find(_.name == DoomViewer.mapComboBox.value.value).get
  }

  private def showQuadTree(level: Level): Unit = {
    val quadTree = level.quadTree
    val lines = fitLinesToScreen(quadTree.getAllBounds)
    val boxes = lines.map(lineToRect(_, Green))
    Platform.runLater {
      boxes.foreach {
        DoomViewer.mapPane.children.add(_)
      }
    }
  }

  // TODO: use this in line conversion code
  private def wadPointToScreenPoint(point: Vertex): Vertex = {
    val (levelBounds, scalingFactor) = getLevelBounds
    val shiftedPoint = Vertex(point.x - levelBounds.a.x, point.y - levelBounds.a.y)
    val flippedPoint = Vertex(shiftedPoint.x, (levelBounds.b.y - levelBounds.a.y) - shiftedPoint.y)
    Vertex((flippedPoint.x / scalingFactor).toInt, (flippedPoint.y / scalingFactor).toInt)
  }

  private def screenPointToWadPoint(x: Double, y: Double): Vertex = {
    //TODO: use getLevelBounds()
    val level = LEVELS.find(_.name == DoomViewer.mapComboBox.value.value).get
    val (minX, minY) = getMinCoords(level.linedefs)
    val (maxX, maxY) = getMaxCoords(level.linedefs)
    val factor = (maxX - minX) / CANVAS_WIDTH
    val scaledPoint = Vertex((x * factor).toInt, (y * factor).toInt)
    val flippedPoint = Vertex(scaledPoint.x, (maxY - minY) - scaledPoint.y)
    Vertex(flippedPoint.x + minX, flippedPoint.y + minY)
  }

  private def lineToRect(line: Linedef, colour: Color): Node = {
    val xCoords = List(line.a.x, line.b.x).sorted
    val yCoords = List(line.a.y, line.b.y).sorted
    val rect = new Rectangle(new JavaFxRectangle(
      xCoords.head,
      yCoords.head,
      xCoords.last - xCoords.head,
      yCoords.last - yCoords.head))
    rect.fill = Transparent
    rect.stroke = colour
    rect
  }

  private def shiftLine(line: Linedef, minX: Double, minY: Double): Linedef = {
    val newAX = line.a.x - minX
    val newAY = line.a.y - minY
    val newBX = line.b.x - minX
    val newBY = line.b.y - minY
    Linedef(Vertex(newAX, newAY), Vertex(newBX, newBY), line.nonTraversable)
  }

  //Doom origin is SW, screen render origin is NW
  private def flipYAxis(line: Linedef, maxY: Double): Linedef = {
    val newAY = maxY - line.a.y
    val newBY = maxY - line.b.y
    Linedef(Vertex(line.a.x, newAY), Vertex(line.b.x, newBY), line.nonTraversable)
  }

  private def scaleLine(line: Linedef, factor: Double): Linedef = {
    val newAX = (line.a.x / factor).toInt
    val newAY = (line.a.y / factor).toInt
    val newBX = (line.b.x / factor).toInt
    val newBY = (line.b.y / factor).toInt
    Linedef(Vertex(newAX, newAY), Vertex(newBX, newBY), line.nonTraversable)
  }

  //TODO: avoid duplicate code
  def getMaxCoords(lines: List[Linedef]): (Double, Double) = {
    val maxStartX = lines.map(_.a.x).max
    val maxEndX = lines.map(_.b.x).max
    val maxStartY = lines.map(_.a.y).max
    val maxEndY = lines.map(_.b.y).max
    val maxX = List(maxStartX, maxEndX).max
    val maxY = List(maxStartY, maxEndY).max
    (maxX, maxY)
  }

  def getMinCoords(lines: List[Linedef]): (Double, Double) = {
    val minStartX = lines.map(_.a.x).min
    val minEndX = lines.map(_.b.x).min
    val minStartY = lines.map(_.a.y).min
    val minEndY = lines.map(_.b.y).min
    val minX = List(minStartX, minEndX).min
    val minY = List(minStartY, minEndY).min
    (minX, minY)
  }

  //TODO: support portrait orientation levels
  private def fitLinesToScreen(lines: List[Linedef]): List[Linedef] = {
    val (levelBounds, scalingFactor) = getLevelBounds
    lines.map(shiftLine(_, levelBounds.a.x, levelBounds.a.y))
      .map(flipYAxis(_, levelBounds.b.y - levelBounds.a.y))
      .map(scaleLine(_, scalingFactor))
  }

  private def getLevelBounds: (Linedef, Double) = {
    val level = LEVELS.find(_.name == DoomViewer.mapComboBox.value.value).get
    val (minX, minY) = getMinCoords(level.linedefs)
    val (maxX, maxY) = getMaxCoords(level.linedefs)
    val factor = (maxX - minX) / CANVAS_WIDTH
    (Linedef(Vertex(minX, minY), Vertex(maxX, maxY), nonTraversable = false), factor)
  }

  private def makeLinesForDisplay(wadLines: List[Linedef], colour: Option[Color] = None,
                                  width: Option[Double] = None): List[Line] = {
    val fittedLines = fitLinesToScreen(wadLines)
    fittedLines.map(line => {
      val l = new Line(new JavaFxLine(line.a.x, line.a.y, line.b.x, line.b.y))
      if (line.nonTraversable) {
        l.strokeWidth = width.getOrElse(2.0)
        l.stroke = colour.getOrElse(Black)
      } else {
        l.strokeWidth = width.getOrElse(1.0)
        l.stroke = colour.getOrElse(Grey)
      }
      l
    })
  }

  def showLevel(level: Level): Unit = {
    Platform.runLater {
      DoomViewer.stage.title = s"Doom Viewer - ${level.name}"
      DoomViewer.mapPane.children = makeLinesForDisplay(level.linedefs)
    }
  }
}

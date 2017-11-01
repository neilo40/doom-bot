import scalafx.scene.paint.Color.{Black, Green, LightGrey, Red, Transparent}
import scalafx.scene.shape.{Circle, Line, Rectangle}
import javafx.scene.shape.{Circle => JavaFxCircle, Line => JavaFxLine, Rectangle => JavaFxRectangle}

import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.scene.Node
import scalafx.scene.paint.Color

object WadViewUtils {
  val CANVAS_WIDTH = 1024
  val CANVAS_HEIGHT = 1024
  val BUTTON_BAR_WIDTH = 200
  var levels: List[Level] = List()

  // Callbacks

  def loadWad(): Unit = {
    val wad = WadParser.createWad()
    levels = wad.levels
    val levelNames = levels.map(_.name)
    Platform.runLater {
      DoomViewer.mapComboBox.items = ObservableBuffer(levelNames)
      DoomViewer.mapComboBox.value = levels.head.name // default to the first map
    }
  }

  def changeLevel(name: String): Unit = {
    val level = levels.find(_.name == name)
    showLevel(level.getOrElse(levels.head))
  }

  def boundingBoxToggle(showBoxes: Boolean): Unit = {
    val level = levels.find(_.name == DoomViewer.mapComboBox.value.value).get
    if (showBoxes){
      val lines = fitLinesToScreen(LineMXQuadTree.allExternalLines(level))
      val boxes = lines.map(lineToRect(_, Red))
      boxes.foreach{DoomViewer.mapPane.children.add(_)}
    } else {
      showLevel(level)
    }
  }

  def quadTreeToggle(shouldShowQuadTree: Boolean): Unit = {
    val level = levels.find(_.name == DoomViewer.mapComboBox.value.value).get
    if (shouldShowQuadTree) showQuadTree(level)
    else showLevel(level)
  }

  def paneClicked(x: Double, y: Double): Unit = {
    if (DoomViewer.showQuadTreeButton.selected.value) {
      val level = levels.find(_.name == DoomViewer.mapComboBox.value.value).get
      val wadPoint = screenPointToWadPoint(x, y)
      val lines = level.quadTree.get.getLinesForPoint(wadPoint)
      if (lines.nonEmpty) {
        showLevel(level)
        showQuadTree(level)
        val screenLines = makeLinesForDisplay(lines, Red)
        screenLines.foreach {
          DoomViewer.mapPane.children.add(_)
        }
      }
    }
  }

  def generatePath(): Unit = {
    val level = levels.find(_.name == DoomViewer.mapComboBox.value.value).get
    val pathNodes = GraphBuilder.genGraphForLevel(level, drawPath = true)
  }

  def drawPathLine(line: WadLine): Unit = {
    val pathLine = makeLinesForDisplay(List(line), otherColour = Green).head
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

  def clearScreen(): Unit = {
    val level = levels.find(_.name == DoomViewer.mapComboBox.value.value).get
    Platform.runLater {
      showLevel(level)
    }
  }

  def startBot(): Unit = {
    val level = levels.find(_.name == DoomViewer.mapComboBox.value.value).get
    val startingNode = GraphBuilder.genGraphForLevel(level)
    PlayerController.startBot(startingNode, level)
  }

  // Private methods

  private def showQuadTree(level: Level): Unit = {
    val quadTree = level.quadTree.get
    val lines = fitLinesToScreen(quadTree.getAllBounds)
    val boxes = lines.map(lineToRect(_, Green))
    boxes.foreach{DoomViewer.mapPane.children.add(_)}
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
    val level = levels.find(_.name == DoomViewer.mapComboBox.value.value).get
    val (minX, minY) = getMinCoords(level.lines.get)
    val (maxX, maxY) = getMaxCoords(level.lines.get)
    val factor = (maxX - minX) / CANVAS_WIDTH
    val scaledPoint = Vertex((x * factor).toInt, (y * factor).toInt)
    val flippedPoint = Vertex(scaledPoint.x, (maxY - minY) - scaledPoint.y)
    Vertex(flippedPoint.x + minX, flippedPoint.y + minY)
  }

  private def lineToRect(line: WadLine, colour: Color): Node = {
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

  private def shiftLine(line: WadLine, minX: Double, minY: Double): WadLine = {
    val newAX = line.a.x - minX
    val newAY = line.a.y - minY
    val newBX = line.b.x - minX
    val newBY = line.b.y - minY
    WadLine(Vertex(newAX, newAY), Vertex(newBX, newBY), line.oneSided)
  }

  //Doom origin is SW, screen render origin is NW
  private def flipYAxis(line: WadLine, maxY: Double): WadLine = {
    val newAY = maxY - line.a.y
    val newBY = maxY - line.b.y
    WadLine(Vertex(line.a.x, newAY), Vertex(line.b.x, newBY), line.oneSided)
  }

  private def scaleLine(line: WadLine, factor: Double): WadLine = {
    val newAX = (line.a.x / factor).toInt
    val newAY = (line.a.y / factor).toInt
    val newBX = (line.b.x / factor).toInt
    val newBY = (line.b.y / factor).toInt
    WadLine(Vertex(newAX, newAY), Vertex(newBX, newBY), line.oneSided)
  }

  //TODO: avoid duplicate code
  def getMaxCoords(lines: List[WadLine]): (Double, Double) = {
    val maxStartX = lines.map(_.a.x).max
    val maxEndX = lines.map(_.b.x).max
    val maxStartY = lines.map(_.a.y).max
    val maxEndY = lines.map(_.b.y).max
    val maxX = List(maxStartX, maxEndX).max
    val maxY = List(maxStartY, maxEndY).max
    (maxX, maxY)
  }

  def getMinCoords(lines: List[WadLine]): (Double, Double) = {
    val minStartX = lines.map(_.a.x).min
    val minEndX = lines.map(_.b.x).min
    val minStartY = lines.map(_.a.y).min
    val minEndY = lines.map(_.b.y).min
    val minX = List(minStartX, minEndX).min
    val minY = List(minStartY, minEndY).min
    (minX, minY)
  }

  //TODO: support portrait orientation levels
  private def fitLinesToScreen(lines: List[WadLine]): List[WadLine] = {
    val (levelBounds, scalingFactor) = getLevelBounds
    lines.map(shiftLine(_, levelBounds.a.x, levelBounds.a.y))
      .map(flipYAxis(_, levelBounds.b.y - levelBounds.a.y))
      .map(scaleLine(_, scalingFactor))
  }

  private def getLevelBounds: (WadLine, Double) = {
    val level = levels.find(_.name == DoomViewer.mapComboBox.value.value).get
    val (minX, minY) = getMinCoords(level.lines.get)
    val (maxX, maxY) = getMaxCoords(level.lines.get)
    val factor = (maxX - minX) / CANVAS_WIDTH
    (WadLine(Vertex(minX, minY), Vertex(maxX, maxY), oneSided = false), factor)
  }

  private def makeLinesForDisplay(wadLines: List[WadLine], colour: Color = Black,
                                  otherColour: Color = LightGrey): List[Line] = {
    val fittedLines = fitLinesToScreen(wadLines)
    fittedLines.map(line => {
      val l = new Line(new JavaFxLine(line.a.x, line.a.y, line.b.x, line.b.y))
      if (line.oneSided) {
        l.strokeWidth = 2
        l.stroke = colour
      } else {
        l.stroke = otherColour
      }
      l
    })
  }

  def showLevel(level: Level): Unit = {
    DoomViewer.stage.title = s"Doom Viewer - ${level.name}"
    DoomViewer.mapPane.children = makeLinesForDisplay(level.lines.get)
  }
}

import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color.LightGrey
import scalafx.scene.shape.Line
import javafx.scene.shape.{Line => JavaFxLine}

import scalafx.collections.ObservableBuffer
import scalafx.scene.control.ComboBox

object WadViewUtils {
  val CANVAS_WIDTH = 1024
  val CANVAS_HEIGHT = 1024
  val BUTTON_BAR_WIDTH = 200
  var levels: List[Level] = List()

  def shiftLine(line: Line, minX: Double, minY: Double): Line = {
    line.setStartX(line.startX.toDouble - minX)
    line.setStartY(line.startY.toDouble - minY)
    line.setEndX(line.endX.toDouble - minX)
    line.setEndY(line.endY.toDouble - minY)
    line
  }

  def flipYAxis(line: Line, maxY: Double): Line = {
    line.setStartY(maxY - line.startY.toDouble)
    line.setEndY(maxY - line.endY.toDouble)
    line
  }

  def scaleLine(line: Line, factor: Double): Line = {
    line.setStartX(line.startX.toDouble / factor)
    line.setStartY(line.startY.toDouble / factor)
    line.setEndX(line.endX.toDouble / factor)
    line.setEndY(line.endY.toDouble / factor)
    line
  }

  //TODO: avoid duplicate code
  def getMaxCoords(lines: List[Line]): (Double, Double) = {
    val maxStartX = lines.map(_.startX.toDouble).max
    val maxEndX = lines.map(_.endX.toDouble).max
    val maxStartY = lines.map(_.startY.toDouble).max
    val maxEndY = lines.map(_.endY.toDouble).max
    val maxX = List(maxStartX, maxEndX).max
    val maxY = List(maxStartY, maxEndY).max
    (maxX, maxY)
  }

  def getMinCoords(lines: List[Line]): (Double, Double) = {
    val minStartX = lines.map(_.startX.toDouble).min
    val minEndX = lines.map(_.endX.toDouble).min
    val minStartY = lines.map(_.startY.toDouble).min
    val minEndY = lines.map(_.endY.toDouble).min
    val minX = List(minStartX, minEndX).min
    val minY = List(minStartY, minEndY).min
    (minX, minY)
  }

  //TODO: support portrait orientation levels
  def fitLinesToScreen(lines: List[Line]): List[Line] = {
    val (minX, minY) = getMinCoords(lines)
    val (maxX, maxY) = getMaxCoords(lines)
    val factor = (maxX - minX) / CANVAS_WIDTH
    lines.map(shiftLine(_, minX.toDouble, minY.toDouble))
      .map(flipYAxis(_, maxY - minY))
      .map(scaleLine(_, factor))
  }

  def loadWad(mapPane: Pane, mapComboBox: ComboBox[String]): Unit = {
    val wad = WadParser.createWad()
    levels = wad.levels
    val levelNames = levels.map(_.name)
    mapComboBox.items = ObservableBuffer(levelNames)
    mapComboBox.value = levels.head.name
  }

  def changeLevel(mapPane: Pane, name: String): Unit = {
    val level = levels.find(_.name == name)
    showLevel(mapPane, level.getOrElse(levels.head))
  }

  def showLevel(mapPane: Pane, level: Level): Unit = {
    DoomViewer.stage.title = s"Doom Viewer - ${level.name}"
    val lines: List[Line] = level.lines.get.map(line => {
      val l = new Line(new JavaFxLine(line.a.x, line.a.y, line.b.x, line.b.y))
      if (line.oneSided) {
        l.strokeWidth = 2
      } else {
        l.stroke = LightGrey
      }
      l
    })
    mapPane.children = fitLinesToScreen(lines)
  }
}

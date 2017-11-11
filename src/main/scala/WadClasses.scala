import scala.math.{pow, sqrt, abs}

case class Wad(wadType: String, levels: List[Level]) {
  override def toString: String = "[Wad] type: " + wadType + ", levels: " + levels
}

case class Level(name: String,
                 linedefs: List[Linedef],
                 quadTree: LineMXQuadTree,
                 start: Vertex,
                 exit: Vertex,
                 doorSwitches: List[DoorSwitch]) {

  def addLines(lines: List[Linedef]): Level = recreateQuadtree(lines)

  def recreateQuadtree(lines: List[Linedef]): Level = {
    val newLines = linedefs ::: lines
    val newQuadTree = LineMXQuadTree.createQuadTree(newLines)
    Level(name, newLines, newQuadTree, start, exit, doorSwitches)
  }

  override def toString: String = "[Level] name: " + name
}

case class Lump(name: String, data: List[Byte]) {
  override def toString: String = "[Lump] name: " + name
}

case class Linedef(a: Vertex,
                   b: Vertex,
                   nonTraversable: Boolean,
                   sectorTag: Option[Int] = None,
                   lineType: Option[Int] = None,
                   rightSideDef: Option[Sidedef] = None,
                   leftSideDef: Option[Sidedef] = None) {

  override def toString: String = s"[Line] $a <-> $b, non-traversable: $nonTraversable"

  def intersectsWith(that: Linedef): Boolean = {
    val denom: Double = (that.b.y - that.a.y) * (this.b.x - this.a.x) - (that.b.x - that.a.x) * (this.b.y - this.a.y)
    if (denom == 0.0) return false
    val ua: Double = ((that.b.x - that.a.x) * (this.a.y - that.a.y) - (that.b.y - that.a.y) * (this.a.x - that.a.x)) / denom
    val ub: Double = ((this.b.x - this.a.x) * (this.a.y - that.a.y) - (this.b.y - this.a.y) * (this.a.x - that.a.x)) / denom
    if (ua >= 0.0 && ua <= 1.0 && ub >= 0.0 && ub <= 1.0) return true
    false
  }

  def midpoint: Vertex = {
    val midX = this.a.x + (this.b.x - this.a.x) / 2
    val midY = this.a.y + (this.b.y - this.a.y) / 2
    Vertex(midX, midY)
  }
}

case class DoorSwitch(a: Vertex, b: Vertex, var switched: Boolean = false) {
  def midpoint: Vertex = {
    val midX = this.a.x + (this.b.x - this.a.x) / 2
    val midY = this.a.y + (this.b.y - this.a.y) / 2
    Vertex(midX, midY)
  }
}

object DoorSwitch{
  def fromWadLine(wadLine: Linedef): DoorSwitch = DoorSwitch(wadLine.a, wadLine.b)
}

case class Vertex(x: Double, y: Double) {
  override def toString: String = s"($x, $y)"

  def +(that: Vertex): Vertex = {
    Vertex(x + that.x, y + that.y)
  }

  def isCloseTo(that: Vertex, range: Double = 400): Boolean = {
    val distance = sqrt(pow(that.y - this.y, 2) + pow(that.x - this.x, 2))
    distance < range
  }

  def distanceTo(that: Vertex): Double = {
    val height = that.y - this.y
    val width  = that.x - this.x
    sqrt((height * height) + (width * width))
  }
}

case class Thing(position: Vertex, facing: Int, doomId: Int)

case class Sector(sectorType: Int, tag: Int, floorHeight: Int, ceilingHeight: Int)

case class Sidedef(sectorTag: Int, sector: Option[Sector])
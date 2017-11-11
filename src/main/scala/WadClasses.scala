import scala.math.{pow, sqrt, abs}

case class Wad(wadType: String, numLumps: Int, levels: List[Level]) {
  override def toString: String = "[Wad] type: " + wadType + ", lumps: " + numLumps + ", levels: " + levels
}

case class Level(name: String,
                 lumps: Map[String, Lump],
                 lines: Option[List[WadLine]] = None,
                 var quadTree: Option[LineMXQuadTree] = None,
                 var playerStart: Option[Vertex] = None,
                 var sectors: Option[List[Sector]] = None,
                 var sideDefs: Option[List[SideDef]] = None,
                 var exit: Option[Vertex] = None,
                 var doorSwitches: Option[List[DoorSwitch]] = None) {

  def addLump(lump: Lump): Level = {
    val newLumps: Map[String, Lump] = lumps + (lump.name -> lump)
    Level(this.name, newLumps, this.lines, this.quadTree, this.playerStart, this.sectors, this.sideDefs, this.exit, this.doorSwitches)
  }

  def setLines(lines: List[WadLine]): Level =
    Level(this.name, this.lumps, Some(lines), this.quadTree, this.playerStart, this.sectors, this.sideDefs, this.exit, this.doorSwitches)

  def addLines(lines: List[WadLine]): Level =
    Level(this.name, this.lumps, Some(lines ::: this.lines.getOrElse(List())), this.quadTree, this.playerStart,
      this.sectors, this.sideDefs, this.exit, this.doorSwitches)

  def setSectors(sectors: List[Sector]): Level =
    Level(this.name, this.lumps, this.lines, this.quadTree, this.playerStart, Some(sectors), this.sideDefs, this.exit, this.doorSwitches)

  def setSideDefs(sideDefs: List[SideDef]): Level =
    Level(this.name, this.lumps, this.lines, this.quadTree, this.playerStart, this.sectors, Some(sideDefs), this.exit, this.doorSwitches)

  def setPlayerStart(v: Vertex): Unit = this.playerStart = Some(v)

  def setExit(v: Vertex): Unit = this.exit = Some(v)

  def setDoorSwitches(v: List[DoorSwitch]): Unit = this.doorSwitches = Some(v)

  override def toString: String = "[Level] name: " + name
}

case class Lump(name: String, data: List[Byte]) {
  override def toString: String = "[Lump] name: " + name
}

case class WadLine(a: Vertex,
                   b: Vertex,
                   nonTraversable: Boolean,
                   sectorTag: Option[Int] = None,
                   lineType: Option[Int] = None,
                   rightSideDef: Option[SideDef] = None,
                   leftSideDef: Option[SideDef] = None) {

  override def toString: String = s"[Line] $a <-> $b, non-traversable: $nonTraversable"

  def intersectsWith(that: WadLine): Boolean = {
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
  def fromWadLine(wadLine: WadLine): DoorSwitch = DoorSwitch(wadLine.a, wadLine.b)
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
}

case class Thing(position: Vertex, facing: Int, doomId: Int)

case class Sector(sectorType: Int, tag: Int, floorHeight: Int, ceilingHeight: Int)

case class SideDef(sectorTag: Int, sector: Option[Sector])
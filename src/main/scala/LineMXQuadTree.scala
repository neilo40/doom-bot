class LineMXQuadTree(cellBounds: WadLine,
                     parent: Option[LineMXQuadTree] = None,
                     depth: Int = 0){
  var content: List[WadLine] = List()
  private var nw: Option[LineMXQuadTree] = None
  private var ne: Option[LineMXQuadTree] = None
  private var se: Option[LineMXQuadTree] = None
  private var sw: Option[LineMXQuadTree] = None

  //Doom origin is SW
  if (depth < LineMXQuadTree.MAX_DEPTH){
    nw = Some(new LineMXQuadTree(
      WadLine(Vertex(cellBounds.a.x, (cellBounds.a.y + cellBounds.b.y) / 2), Vertex((cellBounds.a.x + cellBounds.b.x) / 2, cellBounds.b.y), oneSided = false),
      Some(this),
      depth + 1))
    ne = Some(new LineMXQuadTree(
      WadLine(Vertex((cellBounds.a.x + cellBounds.b.x) / 2, (cellBounds.a.y + cellBounds.b.y) / 2), cellBounds.b, oneSided = false),
      Some(this),
      depth + 1))
    se = Some(new LineMXQuadTree(
      WadLine(Vertex((cellBounds.a.x + cellBounds.b.x) / 2, cellBounds.a.y), Vertex(cellBounds.b.x, (cellBounds.a.y + cellBounds.b.y) / 2), oneSided = false),
      Some(this),
      depth + 1))
    sw = Some(new LineMXQuadTree(
      WadLine(cellBounds.a, Vertex((cellBounds.a.x + cellBounds.b.x) / 2, (cellBounds.a.y + cellBounds.b.y) / 2), oneSided = false),
      Some(this),
      depth + 1))
  }

  def getAllBounds: List[WadLine] = {
    depth match {
      case LineMXQuadTree.MAX_DEPTH => List(cellBounds)
      case _ => nw.get.getAllBounds ++ ne.get.getAllBounds ++ se.get.getAllBounds ++ sw.get.getAllBounds
    }
  }

  def isLineWithinBounds(line: WadLine): Boolean = isPointWithinBounds(line.a) || isPointWithinBounds(line.b)

  def isPointWithinBounds(point: Vertex): Boolean = {
    val pointIsGreaterThanBoundsStart = point.x >= cellBounds.a.x && point.y >= cellBounds.a.y
    val pointIsSmallerThanBoundsEnd = point.x <= cellBounds.b.x && point.y <= cellBounds.b.y
    pointIsGreaterThanBoundsStart && pointIsSmallerThanBoundsEnd
  }

  def assignLineToNode(line: WadLine): Unit = {
    if (depth == LineMXQuadTree.MAX_DEPTH)
      content = line :: content
    else
      List(nw, ne, se, sw) foreach{ quad =>
        if (quad.get.isLineWithinBounds(line)){
          quad.get.assignLineToNode(line)
        }
      }
  }

  def getLinesForPoint(point: Vertex): List[WadLine] = {
    if (depth == LineMXQuadTree.MAX_DEPTH)
      content
    else
      List(nw, ne, se, sw).flatMap(quad =>
        if (quad.get.isPointWithinBounds(point)) quad.get.getLinesForPoint(point)
        else List()
      )
  }

  def getLinesBetweenTwoPoints(start: Vertex, end: Vertex): List[WadLine] = {
    val linesForStartPoint = getLinesForPoint(start).toSet
    val allLines = linesForStartPoint.union(getLinesForPoint(end).toSet)
    allLines.toList
  }
}

object LineMXQuadTree {
  val MAX_DEPTH = 3

  def createQuadTree(level: Level): LineMXQuadTree = {
    val (maxX, maxY) = WadViewUtils.getMaxCoords(level.lines.get)
    val (minX, minY) = WadViewUtils.getMinCoords(level.lines.get)
    val levelBounds = WadLine(Vertex(minX, minY), Vertex(maxX, maxY), oneSided = false)
    val quadTree = new LineMXQuadTree(levelBounds)
    val outsideLines = allExternalLines(level)
    outsideLines foreach quadTree.assignLineToNode
    quadTree
  }

  def allExternalLines(level: Level): List[WadLine] = level.lines.get.filter(l => l.oneSided)
}
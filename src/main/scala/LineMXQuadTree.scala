class LineMXQuadTree(cellBounds: Linedef,
                     parent: Option[LineMXQuadTree] = None,
                     depth: Int = 0){
  var content: List[Linedef] = List()
  private var nw: Option[LineMXQuadTree] = None
  private var ne: Option[LineMXQuadTree] = None
  private var se: Option[LineMXQuadTree] = None
  private var sw: Option[LineMXQuadTree] = None

  //Doom origin is SW
  if (depth < LineMXQuadTree.MAX_DEPTH){
    nw = Some(new LineMXQuadTree(
      Linedef(Vertex(cellBounds.a.x, (cellBounds.a.y + cellBounds.b.y) / 2), Vertex((cellBounds.a.x + cellBounds.b.x) / 2, cellBounds.b.y), nonTraversable = false),
      Some(this),
      depth + 1))
    ne = Some(new LineMXQuadTree(
      Linedef(Vertex((cellBounds.a.x + cellBounds.b.x) / 2, (cellBounds.a.y + cellBounds.b.y) / 2), cellBounds.b, nonTraversable = false),
      Some(this),
      depth + 1))
    se = Some(new LineMXQuadTree(
      Linedef(Vertex((cellBounds.a.x + cellBounds.b.x) / 2, cellBounds.a.y), Vertex(cellBounds.b.x, (cellBounds.a.y + cellBounds.b.y) / 2), nonTraversable = false),
      Some(this),
      depth + 1))
    sw = Some(new LineMXQuadTree(
      Linedef(cellBounds.a, Vertex((cellBounds.a.x + cellBounds.b.x) / 2, (cellBounds.a.y + cellBounds.b.y) / 2), nonTraversable = false),
      Some(this),
      depth + 1))
  }

  def getAllBounds: List[Linedef] = {
    depth match {
      case LineMXQuadTree.MAX_DEPTH => List(cellBounds)
      case _ => nw.get.getAllBounds ++ ne.get.getAllBounds ++ se.get.getAllBounds ++ sw.get.getAllBounds
    }
  }

  def isLineWithinBounds(line: Linedef): Boolean = {
    isPointWithinBounds(line.a) || isPointWithinBounds(line.b) ||
    isPointWithinBounds(Vertex(line.a.x, line.b.y)) || isPointWithinBounds(Vertex(line.b.x, line.a.y)) ||
    lineSpansBounds(line)
  }

  def lineSpansBounds(line: Linedef): Boolean = {
    lineSpansBoundsVertically(line) || lineSpansBoundsHorizontally(line)
  }

  def lineSpansBoundsVertically(line: Linedef): Boolean = {
    //Fudge to catch lines that don't start or end inside, but span vertically or horizontally
    line.a.x > cellBounds.a.x && line.a.x < cellBounds.b.x &&
    line.b.x > cellBounds.a.x && line.b.x < cellBounds.b.x &&
    ((line.a.y > cellBounds.b.y && line.b.y < cellBounds.a.y) ||
     (line.b.y > cellBounds.b.y && line.a.y < cellBounds.a.y))
  }

  def lineSpansBoundsHorizontally(line: Linedef): Boolean = {
    //Fudge to catch lines that don't start or end inside, but span vertically or horizontally
    line.a.y > cellBounds.a.y && line.a.y < cellBounds.b.y &&
      line.b.y > cellBounds.a.y && line.b.y < cellBounds.b.y &&
      ((line.a.x > cellBounds.b.x && line.b.x < cellBounds.a.x) ||
        (line.b.x > cellBounds.b.x && line.a.x < cellBounds.a.x))
  }

  def isPointWithinBounds(point: Vertex): Boolean = {
    val pointIsGreaterThanBoundsStart = point.x >= cellBounds.a.x && point.y >= cellBounds.a.y
    val pointIsSmallerThanBoundsEnd = point.x <= cellBounds.b.x && point.y <= cellBounds.b.y
    pointIsGreaterThanBoundsStart && pointIsSmallerThanBoundsEnd
  }

  def assignLineToNode(line: Linedef): Unit = {
    if (depth == LineMXQuadTree.MAX_DEPTH)
      content = line :: content
    else
      List(nw, ne, se, sw) foreach{ quad =>
        if (quad.get.isLineWithinBounds(line)){
          quad.get.assignLineToNode(line)
        }
      }
  }

  def getLinesForPoint(point: Vertex): List[Linedef] = {
    if (depth == LineMXQuadTree.MAX_DEPTH)
      content
    else
      List(nw, ne, se, sw).flatMap(quad =>
        if (quad.get.isPointWithinBounds(point)) quad.get.getLinesForPoint(point)
        else List()
      )
  }

  def getLinesBetweenTwoPoints(start: Vertex, end: Vertex): List[Linedef] = {
    val linesForStartPoint = getLinesForPoint(start).toSet
    val allLines = linesForStartPoint.union(getLinesForPoint(end).toSet)
    allLines.toList
  }
}

object LineMXQuadTree {
  val MAX_DEPTH = 3

  def createQuadTree(linedefs: List[Linedef]): LineMXQuadTree = {
    val (maxX, maxY) = ViewController.getMaxCoords(linedefs)
    val (minX, minY) = ViewController.getMinCoords(linedefs)
    val levelBounds = Linedef(Vertex(minX - 1, minY - 1), Vertex(maxX + 1, maxY + 1), nonTraversable = false)
    val quadTree = new LineMXQuadTree(levelBounds)
    val outsideLines = allExternalLines(linedefs)
    outsideLines foreach quadTree.assignLineToNode
    quadTree
  }

  def allExternalLines(linedefs: List[Linedef]): List[Linedef] = linedefs.filter(l => l.nonTraversable)
}

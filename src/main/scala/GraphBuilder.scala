import scalafx.scene.paint.Color.Blue

trait PathNodeTrait

class PathNode(location: Vertex,
               level: Level,
               var edges: Map[String, Option[PathNodeTrait]] = Map("n" -> None, "e" -> None, "s" -> None, "w" -> None))
  extends PathNodeTrait {

  def getLocation: Vertex = location
  def getLevel: Level = level

  def north: Option[PathNode] = nodeAtDirection("n")
  def south: Option[PathNode] = nodeAtDirection("s")
  def east: Option[PathNode] = nodeAtDirection("e")
  def west: Option[PathNode] = nodeAtDirection("w")

  def neighbours: List[PathNode] = (edges.values collect { case Some(x: PathNode) => x }).toList

  private def nodeAtDirection(direction: String): Option[PathNode] = {
    edges(direction) match {
      case Some(x: PathNode) => Some(x)
      case _ => None
    }
  }

  // Consider the nodes to be equal if they're in the same place
  override def equals(that: scala.Any): Boolean =
    that match {
      case that: PathNode => this.location.x == that.getLocation.x && this.location.y == that.getLocation.y
      case _ => false
    }

  def branchOut(seenNodes: List[PathNode], drawPath: Boolean): List[PathNode] = {
    edges = edges map {
      case (direction, None) => (direction, Some(addNewNode(location, direction, this, seenNodes, drawPath)))
      case (direction, Some(x)) => (direction, Some(x))
    }
    val pathNodes = extractPathNodes(edges.values.toList.map(_.get))
    pathNodes
  }

  def extractPathNodes(allNodes: List[PathNodeTrait]): List[PathNode] = allNodes collect { case s: PathNode => s }

  //TODO: handle linking existing node differently (it may exist, but may not have path
  def addNewNode(start: Vertex, direction: String, fromNode: PathNode, seenNodes: List[PathNode], drawPath: Boolean): PathNodeTrait = {
    val proposedLocation = start + PathNode.translations(direction)
    val proposedNode = new PathNode(proposedLocation, level)
    val existingProposedNode = seenNodes.find(_ == proposedNode)
    val proposedLine = PathNode.getDirectPath(start, proposedLocation, level)
    if (proposedLine.isEmpty) return new DeadEnd()
    if (drawPath) {
      ViewController.drawPathLine(proposedLine.get)
      ViewController.drawNode(proposedLocation)
    }

    val newNode = if (existingProposedNode.isDefined) existingProposedNode.get else proposedNode
    newNode.edges = newNode.edges + (PathNode.oppositeDirection(direction) -> Some(fromNode))
    newNode
  }

  def isCloseEnough(that: PathNode, excludeSwitchWalls: Boolean = false): Boolean = {
    location.isCloseTo(that.getLocation, GraphBuilder.STEP_SIZE) &&
      PathNode.getDirectPath(location, that.getLocation, level, excludeSwitchWalls).isDefined
  }

  def isCloseEnoughToUse(that: PathNode): Boolean = this.getLocation.isCloseTo(that.getLocation, 100)

  override def toString: String = s"PathNode: $location"
}

object PathNode {
  val oppositeDirection = Map("n" -> "s", "e" -> "w", "s" -> "n", "w" -> "e")
  val translations = Map("n" -> Vertex(0, GraphBuilder.STEP_SIZE),
  "s" -> Vertex(0, -GraphBuilder.STEP_SIZE),
  "e" -> Vertex(GraphBuilder.STEP_SIZE, 0),
  "w" -> Vertex(-GraphBuilder.STEP_SIZE, 0))

  def getDirectPath(start: Vertex, proposedLocation: Vertex, level: Level, excludeSwitchWalls: Boolean = false): Option[Linedef] = {
    val potentialObstructions = level.quadTree.getLinesBetweenTwoPoints(start, proposedLocation)
    val wallList = if (excludeSwitchWalls)
      potentialObstructions.filter(line => !WadParser.SWITCH_TYPES.contains(line.lineType.getOrElse(0)))
    else potentialObstructions
    val proposedLine = Linedef(start, proposedLocation, nonTraversable = false)
    wallList foreach {wall =>
      if (proposedLine.intersectsWith(wall)) return None
    }
    Some(proposedLine)
  }
}

class DeadEnd extends PathNodeTrait

object GraphBuilder {

  val STEP_SIZE = 50
  val MAX_ITERATIONS = 180

  def genGraphForLevel(level: Level, drawPath: Boolean = false): PathNode = {
    val startingPos = new PathNode(level.start, level)
    if (drawPath) {
      ViewController.drawNode(startingPos.getLocation, Blue)
      ViewController.drawNode(level.exit, Blue)
    }
    var newNodes: List[PathNode] = List(startingPos)
    var seenNodes: List[PathNode] = List()
    var iterationCount = 0
    while (iterationCount < MAX_ITERATIONS && newNodes.nonEmpty) {
      newNodes = branchOutNewNodes(newNodes, seenNodes, drawPath)
      seenNodes = seenNodes ++ newNodes
      iterationCount = iterationCount + 1
    }
    startingPos
  }

  def branchOutNewNodes(nodes: List[PathNode], seenNodes: List[PathNode], drawPath: Boolean): List[PathNode] = {
    var seenNodes: List[PathNode] = List()  // nasty, but can't see another way to do this
    val allNewNodes = nodes flatMap { node =>
      val newNodes = node.branchOut(seenNodes, drawPath)
      seenNodes = newNodes ::: seenNodes
      newNodes
    }
    allNewNodes.distinct
  }

}

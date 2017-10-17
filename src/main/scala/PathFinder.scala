import scalafx.scene.paint.Color.Blue

trait PathNodeTrait

class PathNode(location: Vertex,
               level: Level,
               var edges: Map[String, Option[PathNodeTrait]] = Map("n" -> None, "e" -> None, "s" -> None, "w" -> None))
  extends PathNodeTrait {

  def getLocation: Vertex = location

  // Consider the nodes to be equal if they're in the same place
  override def equals(that: scala.Any): Boolean =
    that match {
      case that: PathNode => this.location.x == that.getLocation.x && this.location.y == that.getLocation.y
      case _ => false
    }

  def branchOut(seenNodes: List[PathNode]): List[PathNode] = {
    edges = edges map {
      case (direction, None) => (direction, Some(addNewNode(location, direction, this, seenNodes)))
      case (direction, Some(x)) => (direction, Some(x))
    }
    val pathNodes = extractPathNodes(edges.values.toList.map(_.get))
    pathNodes
  }

  def extractPathNodes(allNodes: List[PathNodeTrait]): List[PathNode] = allNodes collect { case s: PathNode => s }

  //TODO: handle linking existing node differently (it may exist, but may not have path
  def addNewNode(start: Vertex, direction: String, fromNode: PathNode, seenNodes: List[PathNode]): PathNodeTrait = {
    val proposedLocation = start + PathNode.translations(direction)
    val proposedNode = new PathNode(proposedLocation, level)
    val existingProposedNode = seenNodes.find(_ == proposedNode)
    val newNode = if (existingProposedNode.isDefined) existingProposedNode.get else proposedNode
    newNode.edges = newNode.edges + (PathNode.oppositeDirection(direction) -> Some(fromNode))
    val potentialObstructions = level.quadTree.get.getLinesBetweenTwoPoints(start, proposedLocation)
    val proposedLine = WadLine(start, proposedLocation, oneSided = false)
    potentialObstructions foreach {wall =>
      if (proposedLine.intersectsWith(wall)) return new DeadEnd()
    }
    WadViewUtils.drawPath(proposedLine)
    WadViewUtils.drawNode(proposedLocation)
    newNode
  }
}

object PathNode {
  val oppositeDirection = Map("n" -> "s", "e" -> "w", "s" -> "n", "w" -> "e")
  val translations = Map("n" -> Vertex(0, PathFinder.STEP_SIZE),
  "s" -> Vertex(0, -PathFinder.STEP_SIZE),
  "e" -> Vertex(PathFinder.STEP_SIZE, 0),
  "w" -> Vertex(-PathFinder.STEP_SIZE, 0))
}

class DeadEnd extends PathNodeTrait

object PathFinder {

  val STEP_SIZE = 80
  val MAX_ITERATIONS = 80

  def genPathForLevel(level: Level): PathNode = {
    val startingPos = new PathNode(level.playerStart.get, level)
    WadViewUtils.drawNode(startingPos.getLocation, Blue)
    var newNodes: List[PathNode] = List(startingPos)
    var seenNodes: List[PathNode] = List()
    var iterationCount = 0
    while (iterationCount < MAX_ITERATIONS) {
      newNodes = branchOutNewNodes(newNodes, seenNodes)
      seenNodes = seenNodes ++ newNodes
      iterationCount = iterationCount + 1
    }
    startingPos
  }

  def branchOutNewNodes(nodes: List[PathNode], seenNodes: List[PathNode]): List[PathNode] = {
    var seenNodes: List[PathNode] = List()  // nasty, but can't see another way to do this
    val allNewNodes = nodes flatMap { node =>
      val newNodes = node.branchOut(seenNodes)
      seenNodes = newNodes ::: seenNodes
      newNodes
    }
    allNewNodes.distinct
  }

}

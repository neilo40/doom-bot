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
    val potentialObstructions = level.quadTree.get.getLinesBetweenTwoPoints(start, proposedLocation)
    val proposedLine = WadLine(start, proposedLocation, oneSided = false)
    potentialObstructions foreach {wall =>
      if (proposedLine.intersectsWith(wall)) return new DeadEnd()
    }
    if (drawPath) {
      ViewController.drawPathLine(proposedLine)
      ViewController.drawNode(proposedLocation)
    }

    val newNode = if (existingProposedNode.isDefined) existingProposedNode.get else proposedNode
    newNode.edges = newNode.edges + (PathNode.oppositeDirection(direction) -> Some(fromNode))
    newNode
  }

  def isCloseEnough(that: PathNode): Boolean = this.getLocation.isCloseTo(that.getLocation, GraphBuilder.STEP_SIZE)
  def isCloseEnoughToUse(that: PathNode): Boolean = this.getLocation.isCloseTo(that.getLocation, 100)

  override def toString: String = s"PathNode: $location"
}

object PathNode {
  val oppositeDirection = Map("n" -> "s", "e" -> "w", "s" -> "n", "w" -> "e")
  val translations = Map("n" -> Vertex(0, GraphBuilder.STEP_SIZE),
  "s" -> Vertex(0, -GraphBuilder.STEP_SIZE),
  "e" -> Vertex(GraphBuilder.STEP_SIZE, 0),
  "w" -> Vertex(-GraphBuilder.STEP_SIZE, 0))
}

class DeadEnd extends PathNodeTrait

object GraphBuilder {

  val STEP_SIZE = 50
  val MAX_ITERATIONS = 180

  def genGraphForLevel(level: Level, drawPath: Boolean = false): PathNode = {
    val startingPos = new PathNode(level.playerStart.get, level)
    if (drawPath) {
      ViewController.drawNode(startingPos.getLocation, Blue)
      ViewController.drawNode(level.exit.get, Blue)
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

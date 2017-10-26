import scalafx.scene.paint.Color._
import scala.collection.mutable
import scala.math.sqrt

object AStar {

  def getStartingNode: PathNode = {
    def wad = WadParser.createWad()
    GraphBuilder.genGraphForLevel(wad.levels.head)
  }

  // Simple straight line from node to target
  def heuristicCostEstimate(from: PathNode, to: PathNode): Double = {
    val height = to.getLocation.y - from.getLocation.y
    val width  = to.getLocation.x - from.getLocation.x
    sqrt((height * height) + (width * width))
  }

  // 1st attempt, taken straight from wikipedia!
  // TODO: make an immutable, functional version
  def calculatePath(startingNode: PathNode, targetNode: PathNode, drawPathOnly: Boolean = false): Option[PathNode] = {
    val closedSet = mutable.Set[PathNode]()
    val openSet = mutable.Set(startingNode)
    val cameFrom = mutable.Map[PathNode, PathNode]()
    val gScore = mutable.Map(startingNode -> 0.0).withDefaultValue(Double.PositiveInfinity)
    val fScore = mutable.Map(startingNode -> heuristicCostEstimate(startingNode, targetNode))
      .withDefaultValue(Double.PositiveInfinity)

    while (openSet.nonEmpty){
      val currentNode: PathNode = openSet minBy fScore
      if (currentNode.isCloseEnough(targetNode)) {
        reconstructPath(cameFrom, currentNode)
        return Some(currentNode)
      }

      openSet.remove(currentNode)
      closedSet.add(currentNode)
      if (!drawPathOnly) WadViewUtils.drawNode(currentNode.getLocation, Black)

      currentNode.neighbours.foreach { neighbour =>
        if (!closedSet.contains(neighbour)){
          if (!openSet.contains(neighbour)) {
            openSet.add(neighbour)
            if (!drawPathOnly) WadViewUtils.drawNode(neighbour.getLocation, White)
          }
          val tentativeGScore = gScore(currentNode) + heuristicCostEstimate(currentNode, neighbour)
          if (tentativeGScore < gScore(neighbour)){
            cameFrom += (neighbour -> currentNode)
            gScore(neighbour) = tentativeGScore
            fScore(neighbour) = gScore(neighbour) + heuristicCostEstimate(neighbour, targetNode)
          }
        }
      }
    }
    None
  }

  def reconstructPath(cameFrom: mutable.Map[PathNode, PathNode], endNode: PathNode): Unit = {
    var currentNode: PathNode = endNode
    while (cameFrom.contains(currentNode)){
      WadViewUtils.drawNode(currentNode.getLocation, Yellow)
      currentNode = cameFrom(currentNode)
    }
    WadViewUtils.drawNode(currentNode.getLocation, Yellow)
  }

  def closestNodeTo(startingNode: PathNode, target: Vertex): Option[PathNode] = {
    calculatePath(startingNode, new PathNode(target, startingNode.getLevel), drawPathOnly = true)
  }

  def findPathCallback(): Unit = {
    val startingNode = getStartingNode
    //TODO: make this selectable on the UI
    val targetNode = new PathNode(Vertex(1476, -3616), startingNode.getLevel)
    //val targetNode = getStartingNode.north.get.north.get.east.get.north.get
    WadViewUtils.drawNode(targetNode.getLocation, Blue)
    calculatePath(startingNode, targetNode)
  }
}
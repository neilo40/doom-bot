import scalafx.scene.paint.Color._
import scala.collection.mutable

object PathFinder {
  val BARREL_PENALTY = 500

  def getStartingNode(level: Level): PathNode = GraphBuilder.genGraphForLevel(level)

  // Simple straight line from node to target
  def heuristicCostEstimate(from: PathNode, to: PathNode, barrels: List[Linedef]): Double = {
    val score = from.getLocation.distanceTo(to.getLocation)
    val pathLine = Linedef(from.getLocation, to.getLocation, nonTraversable = false)
    barrels foreach { barrel => if (pathLine.intersectsWith(barrel)) return score + BARREL_PENALTY }
    score
  }

  // 1st attempt, taken straight from wikipedia!
  // TODO: make an immutable, functional version
  def calculatePath(startingNode: PathNode, targetNode: PathNode, barrels: List[Linedef], drawPathOnly: Boolean = false,
                    noDraw: Boolean = false): Option[List[PathNode]] = {
    val closedSet = mutable.Set[PathNode]()
    val openSet = mutable.Set(startingNode)
    val cameFrom = mutable.Map[PathNode, PathNode]()
    val gScore = mutable.Map(startingNode -> 0.0).withDefaultValue(Double.PositiveInfinity)
    val fScore = mutable.Map(startingNode -> heuristicCostEstimate(startingNode, targetNode, barrels))
      .withDefaultValue(Double.PositiveInfinity)

    while (openSet.nonEmpty){
      val currentNode: PathNode = openSet minBy fScore
      if (currentNode.isCloseEnough(targetNode, excludeSwitchWalls = true))
        return Some(reconstructPath(cameFrom, currentNode, noDraw))

      openSet.remove(currentNode)
      closedSet.add(currentNode)
      if (!drawPathOnly && !noDraw) ViewController.drawNode(currentNode.getLocation, Black)

      currentNode.neighbours.foreach { neighbour =>
        if (!closedSet.contains(neighbour)){
          if (!openSet.contains(neighbour)) {
            openSet.add(neighbour)
            if (!drawPathOnly && !noDraw) ViewController.drawNode(neighbour.getLocation, White)
          }
          val tentativeGScore = gScore(currentNode) + heuristicCostEstimate(currentNode, neighbour, barrels)
          if (tentativeGScore < gScore(neighbour)){
            cameFrom += (neighbour -> currentNode)
            gScore(neighbour) = tentativeGScore
            fScore(neighbour) = gScore(neighbour) + heuristicCostEstimate(neighbour, targetNode, barrels)
          }
        }
      }
    }
    None
  }

  def reconstructPath(cameFrom: mutable.Map[PathNode, PathNode], endNode: PathNode, noDraw: Boolean): List[PathNode] = {
    var currentNode: PathNode = endNode
    var path: List[PathNode] = List(endNode)
    while (cameFrom.contains(currentNode)){
      if (!noDraw) ViewController.drawNode(currentNode.getLocation, OrangeRed)
      currentNode = cameFrom(currentNode)
      path = currentNode :: path
    }
    if (!noDraw) ViewController.drawNode(currentNode.getLocation, OrangeRed)
    path
  }

  def closestNodeTo(startingNode: PathNode, target: Vertex): Option[PathNode] =
    calculatePath(startingNode, new PathNode(target, startingNode.getLevel), List(), noDraw = true) match {
      case Some(l) => l.lastOption
      case _ => None
    }
}
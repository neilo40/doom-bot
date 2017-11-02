import WadViewUtils.drawNode
import math._
import scalafx.scene.paint.Color.Green

object PlayerController {
  val barrelSize = 60

  def startBot(): Unit = {
    val level = WadViewUtils.levels.find(_.name == DoomViewer.mapComboBox.value.value).get
    val barrelLines = PlayerInterface.getAllBarrels flatMap barrelToLines
    val levelWithBarrels = level.addLines(barrelLines)
    levelWithBarrels.quadTree = Some(LineMXQuadTree.createQuadTree(levelWithBarrels))
    WadViewUtils.showLevel(levelWithBarrels)
    val startingNode = GraphBuilder.genGraphForLevel(levelWithBarrels)
    var currentNode = startingNode
    while (true) currentNode = iterateBot(currentNode, level)
  }

  // Convert a barrel to a bounding box of one-sided lines
  def barrelToLines(barrel: Object): List[WadLine] = {
    val x = barrel.position.x
    val y = barrel.position.y
    List(
      WadLine(Vertex(x - barrelSize, y - barrelSize), Vertex(x + barrelSize, y - barrelSize), oneSided = true),
      WadLine(Vertex(x + barrelSize, y - barrelSize), Vertex(x + barrelSize, y + barrelSize), oneSided = true),
      WadLine(Vertex(x + barrelSize, y + barrelSize), Vertex(x - barrelSize, y + barrelSize), oneSided = true),
      WadLine(Vertex(x - barrelSize, y + barrelSize), Vertex(x - barrelSize, y - barrelSize), oneSided = true)
    )
  }

  // This is the main player loop
  def iterateBot(currentNode: PathNode, level: Level): PathNode = {
    val targetNode = new PathNode(level.exit.get, level)  // This may change during the level, but not for E1M1
    val player = PlayerInterface.getPlayer
    val playerNode = AStar.closestNodeTo(currentNode, player.position).getOrElse(currentNode)
    if (playerNode.isCloseEnough(targetNode)) PlayerInterface.use()
    if (PlayerInterface.isNearClosedDoor(player)) PlayerInterface.use()
    val worldObjects = PlayerInterface.getObjects()
    val threat = PlayerInterface.nearbyThreat(player, worldObjects)
    threat match {
      case Some(t) =>
        turnTowards(player.position, t.position)
        PlayerInterface.shoot()
      case _ =>
        val path = AStar.calculatePath(playerNode, targetNode, noDraw = true)
        val nextNode = getNextNode(path, player)
        headTowards(player.position, nextNode)
    }
    drawScene(level, player)
    playerNode
  }

  // Look ahead in the path and see if we can skip some nodes
  def getNextNode(path: List[PathNode], player: Player): PathNode = {
    var accessibleNode = path.tail.head
    path.slice(1, 5).foreach { node =>
      accessibleNode = if (PlayerInterface.canMoveTo(player, node.getLocation)) node else accessibleNode
    }
    accessibleNode
  }

  def turnTowards(currentPosition: Vertex, targetPosition: Vertex): Unit = {
    val targetAngle = toDegrees(atan2(targetPosition.x - currentPosition.x,
      currentPosition.y - targetPosition.y)) - 90
    val normalisedAngle = if (targetAngle < 0) targetAngle + 360 else targetAngle
    PlayerInterface.turn(normalisedAngle.toInt)
  }

  def headTowards(currentPosition: Vertex, nextNode: PathNode): Unit = {
    drawNode(nextNode.getLocation, Green)
    turnTowards(currentPosition, nextNode.getLocation)
    PlayerInterface.move(10)
  }

  private def drawScene(level: Level, player: Player): Unit = {
    drawNode(player.position)
  }
}

import ViewController.drawNode
import math._
import scalafx.scene.paint.Color._

object PlayerController {
  val BARREL_SIZE = 40
  val LOOK_AHEAD = 6 // How far in the path to look ahead
  var DOOR_SWITCH_TARGET: Option[DoorSwitch] = None
  val DOOR_SWITCH_RANGE = 200
  //var BARREL_KILLER = false  // whether we should be killing barrels or avoiding them

  def startBot(): Unit = {
    val level = ViewController.LEVELS.find(_.name == DoomViewer.mapComboBox.value.value).get
    val barrelLines = PlayerInterface.getAllBarrels flatMap barrelToLines
    val levelWithBarrels = level.addLines(barrelLines)
    levelWithBarrels.quadTree = Some(LineMXQuadTree.createQuadTree(levelWithBarrels))
    ViewController.showLevel(levelWithBarrels)
    val startingNode = GraphBuilder.genGraphForLevel(levelWithBarrels)
    var currentNode = startingNode
    val keys = PlayerInterface.getAllKeys

    while (ViewController.BOT_RUNNING) currentNode = iterateBot(currentNode, level, keys)
  }

  // Convert a barrel to a bounding box of one-sided lines
  def barrelToLines(barrel: Object): List[WadLine] = {
    val x = barrel.position.x
    val y = barrel.position.y
    List(
      WadLine(Vertex(x - BARREL_SIZE, y - BARREL_SIZE), Vertex(x + BARREL_SIZE, y - BARREL_SIZE), nonTraversable = true),
      WadLine(Vertex(x + BARREL_SIZE, y - BARREL_SIZE), Vertex(x + BARREL_SIZE, y + BARREL_SIZE), nonTraversable = true),
      WadLine(Vertex(x + BARREL_SIZE, y + BARREL_SIZE), Vertex(x - BARREL_SIZE, y + BARREL_SIZE), nonTraversable = true),
      WadLine(Vertex(x - BARREL_SIZE, y + BARREL_SIZE), Vertex(x - BARREL_SIZE, y - BARREL_SIZE), nonTraversable = true)
    )
  }

  // This is the main player loop
  def iterateBot(currentNode: PathNode, level: Level, keys: List[Object]): PathNode = {
    val player = PlayerInterface.getPlayer
    if (player.health <= 0) ViewController.BOT_RUNNING = false
    val lockedDoors = PlayerInterface.lockedDoors(player)
    val targetNode = setTarget(lockedDoors, level, player)
    drawNode(targetNode.getLocation, colour = Purple)
    val playerNode = PathFinder.closestNodeTo(currentNode, player.position).getOrElse(currentNode)
    if (playerNode.isCloseEnoughToUse(targetNode) &&
      PathNode.getDirectPath(player.position, targetNode.getLocation, level, excludeSwitchWalls = true).isDefined) {
      // we should stop the bot running here if target node was the exit
      DOOR_SWITCH_TARGET match {
        case Some(x) => x.switched = true
        case _ =>
      }
      PlayerInterface.use()
    }
    if (PlayerInterface.isNearClosedDoor(player)) PlayerInterface.use()
    val worldObjects = PlayerInterface.getObjects()
    val threat = PlayerInterface.nearbyThreat(player, worldObjects)
    threat match {
      case Some(t) =>
        turnTowards(player.position, t.position)
        PlayerInterface.shoot()
      case _ =>
        unStuck(player.position, currentNode.getLocation)
        val path = PathFinder.calculatePath(playerNode, targetNode, noDraw = true)
        val (nextNode, speed) = getNextNode(path, player, level)
        headTowards(player.position, nextNode, speed)
    }
    drawScene(level, player)
    playerNode
  }

  def unStuck(position: Vertex, lastPosition: Vertex): Unit = if (position == lastPosition) PlayerInterface.strafeLeft()

  def setTarget(lockedDoors: List[Door], level: Level, player: Player): PathNode = {
    val doorSwitch = doorSwitchInRange(level, player)
    if (doorSwitch.isDefined) {
      DOOR_SWITCH_TARGET = doorSwitch
      new PathNode(doorSwitch.get.midpoint, level)
    } else if (lockedDoors.nonEmpty){
      val key = PlayerInterface.getKey(lockedDoors.head.keyRequired)
      key match {
        case Some(k) => new PathNode(k.position, level)
        case _ => new PathNode(level.exit.get, level)
      }
    } else {
      new PathNode(level.exit.get, level)
    }
  }

  def doorSwitchInRange(level: Level, player: Player): Option[DoorSwitch] = {
    val doorSwitches = level.doorSwitches.getOrElse(List())
    if (doorSwitches.nonEmpty){
      doorSwitches.filter(!_.switched).foreach { doorSwitch =>
        if (doorSwitch.midpoint.isCloseTo(player.position, DOOR_SWITCH_RANGE)) return Some(doorSwitch)
      }
    }
    None
  }

  // Look ahead in the path and see if we can skip some nodes
  def getNextNode(path: Option[List[PathNode]], player: Player, level: Level): (PathNode, Int) = {
    var speed = 10
    path match {
      case Some(p) =>
        p.slice(1, LOOK_AHEAD).reverse.foreach { node =>
          if (PlayerInterface.canMoveTo(player, node.getLocation))
            return (node, speed)
          else
            speed -= 1
        }
        (p.tail.headOption.getOrElse(p.head), speed)
      case None =>
        println("No path to target")
        (new PathNode(player.position, level), 0)
    }
  }

  def turnTowards(currentPosition: Vertex, targetPosition: Vertex): Unit = {
    val targetAngle = toDegrees(atan2(targetPosition.x - currentPosition.x,
      currentPosition.y - targetPosition.y)) - 90
    val normalisedAngle = if (targetAngle < 0) targetAngle + 360 else targetAngle
    PlayerInterface.turn(normalisedAngle.toInt)
  }

  def headTowards(currentPosition: Vertex, nextNode: PathNode, speed: Int): Unit = {
    drawNode(nextNode.getLocation, Green)
    turnTowards(currentPosition, nextNode.getLocation)
    PlayerInterface.move(speed)
  }

  private def drawScene(level: Level, player: Player): Unit = {
    drawNode(player.position)
  }
}

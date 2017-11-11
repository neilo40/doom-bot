import ViewController.drawNode
import math._
import scalafx.scene.paint.Color._

object PlayerController {
  val BARREL_SIZE = 60
  val BARREL_SIZE_FOR_TARGETING = 30
  val LOOK_AHEAD = 6 // How far in the path to look ahead
  var DOOR_SWITCH_TARGET: Option[DoorSwitch] = None
  val DOOR_SWITCH_RANGE = 200
  val STUCK_THRESHOLD = 10  // while moving, distance we need to have travelled else stuck

  def startBot(): Unit = {
    val level = ViewController.LEVELS.find(_.name == DoomViewer.mapComboBox.value.value).get
    val gameBarrels = GameInterface.getAllBarrels
    val barrels: Map[String, List[Linedef]] = Map(
      "pathing" -> gameBarrels.flatMap(barrelToLines(_, BARREL_SIZE)),
      "targeting" -> gameBarrels.flatMap(barrelToLines(_, BARREL_SIZE_FOR_TARGETING)))
    val levelWithBarrels = level.addLines(barrels("pathing"))
    ViewController.showLevel(levelWithBarrels)
    val startingNode = GraphBuilder.genGraphForLevel(levelWithBarrels)
    var currentNode = startingNode
    val keys = GameInterface.getAllKeys

    while (ViewController.BOT_RUNNING) currentNode = iterateBot(currentNode, level, keys, barrels)
  }

  // Convert a barrel to a bounding box of one-sided lines
  // They are traversable but will incur a large penalty when calculating the path
  def barrelToLines(barrel: Object, size: Int): List[Linedef] = {
    val x = barrel.position.x
    val y = barrel.position.y
    List(
      Linedef(Vertex(x - size, y - size), Vertex(x + size, y - size), nonTraversable = false),
      Linedef(Vertex(x + size, y - size), Vertex(x + size, y + size), nonTraversable = false),
      Linedef(Vertex(x + size, y + size), Vertex(x - size, y + size), nonTraversable = false),
      Linedef(Vertex(x - size, y + size), Vertex(x - size, y - size), nonTraversable = false)
    )
  }

  // This is the main player loop
  def iterateBot(currentNode: PathNode, level: Level, keys: List[Object], barrels: Map[String, List[Linedef]]): PathNode = {
    val player = GameInterface.getPlayer
    if (player.health <= 0) ViewController.BOT_RUNNING = false
    val lockedDoors = GameInterface.lockedDoors(player)
    val targetNode = setTarget(lockedDoors, level, player)
    drawNode(targetNode.getLocation, colour = Purple)
    val playerNode = PathFinder.closestNodeTo(currentNode, player.position).getOrElse(currentNode)
    if (playerNode.isCloseEnoughToUse(targetNode) &&
      PathNode.getDirectPath(player.position, targetNode.getLocation, level, excludeSwitchWalls = true).isDefined) {
      // TODO: we should stop the bot running here if target node was the exit
      DOOR_SWITCH_TARGET match {
        case Some(x) => x.switched = true
        case _ =>
      }
      GameInterface.use()
    }
    if (GameInterface.isNearClosedDoor(player)) GameInterface.use()
    val worldObjects = GameInterface.getObjects()
    val threat = GameInterface.nearbyThreat(player, worldObjects)
    if (threat.isDefined && !aboutToShootBarrel(player, threat.get, barrels("targeting"))) {
      turnTowards(player.position, threat.get.position)
      GameInterface.shoot()
    } else {
      unStuck(player.position, currentNode.getLocation)
      val path = PathFinder.calculatePath(playerNode, targetNode, barrels("pathing"), noDraw = true)
      val (nextNode, speed) = getNextNode(path, player, level)
      headTowards(player.position, nextNode, speed)
    }
    drawScene(level, player)
    playerNode
  }

  // If barrel is between player and target, don't engage!
  def aboutToShootBarrel(player: Player, enemy: Object, barrels: List[Linedef]): Boolean = {
    val lineOfSight = Linedef(player.position, enemy.position, nonTraversable = false)
    barrels foreach { barrel => if (lineOfSight.intersectsWith(barrel)) return true }
    false
  }

  def unStuck(position: Vertex, lastPosition: Vertex): Unit = {
    val distanceTraveled = lastPosition.distanceTo(position)
    if (distanceTraveled < STUCK_THRESHOLD) GameInterface.strafeLeft()
  }

  def setTarget(lockedDoors: List[Door], level: Level, player: Player): PathNode = {
    val doorSwitch = doorSwitchInRange(level, player)
    if (doorSwitch.isDefined) {
      DOOR_SWITCH_TARGET = doorSwitch
      new PathNode(doorSwitch.get.midpoint, level)
    } else if (lockedDoors.nonEmpty){
      val key = GameInterface.getKey(lockedDoors.head.keyRequired)
      key match {
        case Some(k) => new PathNode(k.position, level)
        case _ => new PathNode(level.exit, level)
      }
    } else {
      new PathNode(level.exit, level)
    }
  }

  def doorSwitchInRange(level: Level, player: Player): Option[DoorSwitch] = {
    val doorSwitches = level.doorSwitches
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
          if (GameInterface.canMoveTo(player, node.getLocation))
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
    GameInterface.turn(normalisedAngle.toInt)
  }

  def headTowards(currentPosition: Vertex, nextNode: PathNode, speed: Int): Unit = {
    drawNode(nextNode.getLocation, Green)
    turnTowards(currentPosition, nextNode.getLocation)
    GameInterface.move(speed)
  }

  private def drawScene(level: Level, player: Player): Unit = {
    drawNode(player.position)
  }
}

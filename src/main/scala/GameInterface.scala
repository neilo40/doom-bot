import scalaj.http.{Http, HttpResponse}
import spray.json._
import Protocols._
import math._

object GameInterface {
  val baseUrl = "http://192.168.44.50:6002/api"
  //val baseUrl = "http://localhost:6001/api"
  val enemyIds = List(3004, 3001, 9, 3002)  // 2035=barrel
  val threatThreshold = 400
  val doorThreshold = 150
  val keyCardIds = List(5, 6, 13)

  private def post(action: String, data: String): HttpResponse[String] =
    Http(s"$baseUrl/$action").postData(data).header("content-type", "application/json").asString

  private def patch(action: String, data: String): HttpResponse[String] =
    Http(s"$baseUrl/$action").postData(data).method("PATCH").header("content-type", "application/json").asString

  private def get(action: String): HttpResponse[String] =
    Http(s"$baseUrl/$action").asString

  def changeLevel(episode: Char, map: Char): Unit = {
    val jsonData = "{\"episode\": " + episode + ", \"map\": " + map + "}"
    patch("world", jsonData)
  }

  def getWorldSettings: World = get("world").body.parseJson.convertTo[World]

  def getPlayer: Player = {
    val playerJsonString = get("player").body
    val jsonAst = playerJsonString.parseJson
    jsonAst.convertTo[Player]
  }

  def getPlayers: List[Player] = {
    val playersJsonString = get("players").body
    val jsonAst = playersJsonString.parseJson
    jsonAst.convertTo[List[Player]]
  }

  def selectWeapon(num: Int): Unit = {
    val jsonData = "{\"type\": \"switch-weapon\", \"amount\": " + num + "}"
    post("player/actions", jsonData)
  }

  def move(amount: Int): Unit = {
    val direction = if (amount > 0) "forward" else "backward"
    val abs_amount = abs(amount)
    val jsonData = "{\"type\": \"" + direction + "\", \"amount\": " + abs_amount + "}"
    post("player/actions", jsonData)
  }

  def strafeLeft(): Unit = {
    val jsonData = "{\"type\": \"strafe-left\"}"
    post("player/actions", jsonData)
  }

  def use(): Unit = {
    val jsonData = "{\"type\": \"use\"}"
    post("player/actions", jsonData)
  }

  def shoot(): Unit = {
    val jsonData = "{\"type\": \"shoot\"}"
    post("player/actions", jsonData)
  }

  def turn(angle: Int): Unit = {
    val jsonData = "{\"target_angle\": " + angle + "}"
    post("player/turn", jsonData)
  }

  //Use the shitty turn api to head towards a target
  def turnTowards(player: Player, targetPos: Vertex): Unit = {
    val targetAngle = toDegrees(atan2(targetPos.x - player.position.x,
      player.position.y - targetPos.y)) - 90
    val normalisedAngle = if (targetAngle < 0) targetAngle + 360 else targetAngle
    var remaining = normalisedAngle - player.angle
    if (remaining < 0) remaining = remaining + 360
    val direction = if (remaining < 180) "turn-left" else "turn-right"
    //TODO: move by a smaller amount when the remaining angle is small
    val jsonData = "{\"type\": \"" + direction + "\", \"amount\": 25}"
    post("player/actions", jsonData)
  }

  def canMoveTo(player: Player, target: Vertex): Boolean = {
    val resp = Http(s"$baseUrl/world/movetest").params(
      Seq(("id", player.id.toString),
        ("x", target.x.toString),
        ("y", target.y.toString))
    ).asString
    val moveTestResponse = resp.body.parseJson.convertTo[MoveTestResponse]
    moveTestResponse.result
  }

  def canSee(player: Player, target: Object): Boolean = {
    val resp = get(s"world/los/${player.id}/${target.id}")
    val los = resp.body.parseJson.convertTo[LosResponse]
    los.los
  }

  def isNearClosedDoor(player: Player): Boolean = {
    val resp = Http(s"$baseUrl/world/doors").param("distance", doorThreshold.toString).asString
    val doors = resp.body.parseJson.convertTo[List[Door]]
    doors.nonEmpty && doors.maxBy(_.distance).state == "closed"  // this is not reliable
  }

  // get all doors that are locked for which we don't have the key yet
  def lockedDoors(player: Player): List[Door] = {
    getAllDoors.filter {door =>
      door.keyRequired != "none" && !hasKey(player, door.keyRequired)
    }
  }

  def hasKey(player: Player, key: String): Boolean = {
    key match {
      case "blue" => player.keyCards.blue
      case "red" => player.keyCards.red
      case "yellow" => player.keyCards.yellow
      case _ => false
    }
  }

  def getObjects(distance: Int = threatThreshold): List[Object] = {
    val resp = Http(s"$baseUrl/world/objects").param("distance", distance.toString).asString
    resp.body.parseJson.convertTo[List[Object]]
  }

  def getAllShells: List[Object] = getAllObjects filter(_.typeId == 2008)

  def getAllShotguns: List[Object] = getAllObjects filter(_.typeId == 2001)

  def getAllObjects: List[Object] = get("world/objects").body.parseJson.convertTo[List[Object]]

  def getAllKeys: List[Object] = getAllObjects.filter { obj => keyCardIds.contains(obj.typeId) }

  def getKey(colour: String): Option[Object] =
    getAllKeys find {key => key.typeString.toLowerCase.contains(colour)}

  def getAllBarrels: List[Object] = getAllObjects.filter(_.typeId == 2035)

  def getAllDoors: List[Door] = get("world/doors").body.parseJson.convertTo[List[Door]]

  def nearbyThreat(player: Player, objects: List[Object]): Option[Object] = {
    val aliveEnemies = objects.filter({obj =>
      enemyIds.contains(obj.typeId) && obj.health > 0 && canSee(player, obj)
    })
    aliveEnemies.sortBy(_.distance).headOption
  }

}

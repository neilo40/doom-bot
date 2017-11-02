import scalaj.http.{Http, HttpResponse}
import spray.json._
import Protocols._
import math._

object PlayerInterface {
  val baseUrl = "http://localhost:6001/api"
  val ENEMY_IDS = List(3004, 3001)  // 2035=barrel
  val threatThreshold = 400
  val doorThreshold = 150

  private def post(action: String, data: String): HttpResponse[String] =
    Http(s"$baseUrl/$action").postData(data).header("content-type", "application/json").asString

  private def get(action: String): HttpResponse[String] =
    Http(s"$baseUrl/$action").asString

  def getPlayer: Player = {
    val playerJsonString = get("player").body
    val jsonAst = playerJsonString.parseJson
    jsonAst.convertTo[Player]
  }

  def move(amount: Int): Unit = {
    val direction = if (amount > 0) "forward" else "backward"
    val abs_amount = abs(amount)
    val jsonData = "{\"type\": \"" + direction + "\", \"amount\": " + abs_amount + "}"
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
    doors.nonEmpty && doors.maxBy(_.distance).state == "closed"
  }

  def getObjects(distance: Int = threatThreshold): List[Object] = {
    val resp = Http(s"$baseUrl/world/objects").param("distance", distance.toString).asString
    resp.body.parseJson.convertTo[List[Object]]
  }

  def getAllBarrels: List[Object] = {
    val resp = Http(s"$baseUrl/world/objects").asString
    val allObjects = resp.body.parseJson.convertTo[List[Object]]
    allObjects.filter(_.typeId == 2035)
  }

  def nearbyThreat(player: Player, objects: List[Object]): Option[Object] = {
    val aliveEnemies = objects.filter({obj =>
      ENEMY_IDS.contains(obj.typeId) && obj.health > 0 && canSee(player, obj)
    })
    aliveEnemies.sortBy(_.distance).headOption
  }
}

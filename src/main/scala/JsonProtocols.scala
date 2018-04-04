import spray.json._

case class Player(position: Vertex, angle: Int, id: Int, keyCards: KeyCards, health: Int, consoleplayer: Boolean,
                  weapons: Weapons, ammo: Ammo, weapon: Int)
case class MoveTestResponse(id: Int, x: Int, y: Int, result: Boolean)
case class Door(id: Int, distance: Double, state: String, keyRequired: String, line: DoorLine)
case class Object(position: Vertex, typeId: Int, distance: Double, health: Int, id: Int, typeString: String)
case class LosResponse(los: Boolean, id: Int, id2: Int)
case class KeyCards(blue: Boolean, red: Boolean, yellow: Boolean)
case class DoorLine(v1: Vertex, v2: Vertex)
case class World(map: Int, episode: Int, lights: String, skill: String, wad: String)
case class Weapons(plasma: Boolean, shotgun: Boolean, bfg: Boolean, rocket: Boolean, chainsaw: Boolean, chaingun: Boolean)
case class Ammo(Rockets: Int, Cells: Int, Bullets: Int, Shells: Int)

object Protocols extends DefaultJsonProtocol {
  implicit val vertexFormat: JsonFormat[Vertex] = lazyFormat(jsonFormat(Vertex, "x", "y"))
  implicit val lineFormat: JsonFormat[DoorLine] = lazyFormat(jsonFormat(DoorLine, "v1", "v2"))
  implicit val MoveTestResponseFormat: JsonFormat[MoveTestResponse] = jsonFormat4(MoveTestResponse)
  implicit val LosResponseFormat: JsonFormat[LosResponse] = jsonFormat3(LosResponse)
  implicit val KeyCardsFormat: JsonFormat[KeyCards] = jsonFormat3(KeyCards)
  implicit val WorldFormat: JsonFormat[World] = jsonFormat5(World)
  implicit val AmmoFormat: JsonFormat[Ammo] = jsonFormat4(Ammo)

  implicit object WeaponsFormat extends RootJsonFormat[Weapons] {
    override def write(obj: Weapons): JsValue = throw new RuntimeException("Writing Weapons objects not supported")

    override def read(json: JsValue): Weapons = {
      json.asJsObject.getFields("Plasma Rifle", "Shotgun", "BFG?", "Rocket Launcher", "Chainsaw", "Chaingun", "Handgun") match {
        case Seq(JsBoolean(plasma), JsBoolean(shotgun), JsBoolean(bfg), JsBoolean(rocket), JsBoolean(chainsaw),
        JsBoolean(chaingun), JsBoolean(handgun)) =>
          Weapons(plasma.toJson.convertTo[Boolean], shotgun.toJson.convertTo[Boolean], bfg.toJson.convertTo[Boolean],
            rocket.toJson.convertTo[Boolean], chainsaw.toJson.convertTo[Boolean], chaingun.toJson.convertTo[Boolean])
        case _ => throw DeserializationException("Couldn't deserialize weapons")
      }
    }
  }

  implicit object PlayerJsonFormat extends RootJsonFormat[Player] {
    override def write(obj: Player): JsValue = throw new RuntimeException("Writing Player objects Not supported")

    override def read(json: JsValue): Player = {
      json.asJsObject.getFields("position", "angle", "id", "keyCards", "health", "isConsolePlayer", "weapons", "ammo", "weapon") match {
        case Seq(JsObject(position), JsNumber(angle), JsNumber(id), JsObject(keyCards), JsNumber(health),
        JsBoolean(consoleplayer), JsObject(weapons), JsObject(ammo), JsNumber(weapon)) =>
          Player(position.toJson.convertTo[Vertex], angle.toInt, id.toInt, keyCards.toJson.convertTo[KeyCards],
            health.toInt, consoleplayer.toJson.convertTo[Boolean], weapons.toJson.convertTo[Weapons],
            ammo.toJson.convertTo[Ammo], weapon.toInt)
        case _ => throw DeserializationException("Couldn't deserialize player")
      }
    }
  }

  implicit object ObjectJsonFormat extends RootJsonFormat[Object] {
    override def write(obj: Object): JsValue = throw new RuntimeException("Writing Objects Not supported")

    override def read(json: JsValue): Object = {
      json.asJsObject.getFields("position", "typeId", "distance", "health", "id", "type") match {
        case Seq(JsObject(position), JsNumber(typeId), JsNumber(distance), JsNumber(health), JsNumber(id), JsString(typeString)) =>
          Object(position.toJson.convertTo[Vertex], typeId.toInt, distance.toDouble, health.toInt, id.toInt, typeString.toString)
        case _ => throw DeserializationException("Couldn't deserialize Object")
      }
    }
  }

  implicit object VertexJsonFormat extends RootJsonFormat[Vertex] {
    override def write(obj: Vertex): JsValue = JsObject(
      "x" -> JsNumber(obj.x),
      "y" -> JsNumber(obj.y)
    )

    override def read(json: JsValue): Vertex = {
      json.asJsObject.getFields("x", "y") match {
        case Seq(JsNumber(x), JsNumber(y)) => Vertex(x.toDouble, y.toDouble)
        case _ => throw DeserializationException("Couldn't deserialize vertex")
      }
    }
  }

  implicit object LineJsonFormat extends RootJsonFormat[DoorLine] {
    override def write(obj: DoorLine): JsValue = throw new RuntimeException("Writing Line objects not supported")

    override def read(json: JsValue): DoorLine = {
      json.asJsObject.getFields("v1", "v2") match {
        case Seq(JsObject(v1), JsObject(v2)) => DoorLine(v1.toJson.convertTo[Vertex], v2.toJson.convertTo[Vertex])
        case _ => throw DeserializationException("Couldn't deserialize line")
      }
    }
  }

  implicit object DoorJsonFormat extends RootJsonFormat[Door] {
    override def write(obj: Door): JsValue = throw new RuntimeException("Writing Door objects not supported")

    override def read(json: JsValue): Door = {
      json.asJsObject.getFields("id", "distance", "state", "keyRequired", "line") match {
        case Seq(JsNumber(id), JsNumber(distance), JsString(state), JsString(keyRequired), JsObject(line)) =>
          Door(id.toInt, distance.toDouble, state.toString, keyRequired.toString, line.toJson.convertTo[DoorLine])
        case _ => throw DeserializationException("Couldn't deserialize door")
      }
    }
  }
}

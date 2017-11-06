import spray.json.{DefaultJsonProtocol, DeserializationException, JsNumber, JsObject, JsString, JsValue, JsonFormat, RootJsonFormat}

case class Player(position: Vertex, angle: Int, id: Int, keyCards: KeyCards, health: Int)
case class MoveTestResponse(id: Int, x: Int, y: Int, result: Boolean)
case class Door(id: Int, distance: Double, state: String, keyRequired: String, line: DoorLine)
case class Object(position: Vertex, typeId: Int, distance: Double, health: Int, id: Int, typeString: String)
case class LosResponse(los: Boolean, id: Int, id2: Int)
case class KeyCards(blue: Boolean, red: Boolean, yellow: Boolean)
case class DoorLine(v1: Vertex, v2: Vertex)

object Protocols extends DefaultJsonProtocol {
  implicit val vertexFormat: JsonFormat[Vertex] = lazyFormat(jsonFormat(Vertex, "x", "y"))
  implicit val lineFormat: JsonFormat[DoorLine] = lazyFormat(jsonFormat(DoorLine, "v1", "v2"))
  implicit val MoveTestResponseFormat: JsonFormat[MoveTestResponse] = jsonFormat4(MoveTestResponse)
  implicit val LosResponseFormat: JsonFormat[LosResponse] = jsonFormat3(LosResponse)
  implicit val KeyCardsFormat: JsonFormat[KeyCards] = jsonFormat3(KeyCards)

  implicit object PlayerJsonFormat extends RootJsonFormat[Player] {
    override def write(obj: Player): JsValue = throw new RuntimeException("Writing Player objects Not supported")

    override def read(json: JsValue): Player = {
      json.asJsObject.getFields("position", "angle", "id", "keyCards", "health") match {
        case Seq(JsObject(position), JsNumber(angle), JsNumber(id), JsObject(keyCards), JsNumber(health)) =>
          Player(position.toJson.convertTo[Vertex], angle.toInt, id.toInt, keyCards.toJson.convertTo[KeyCards], health.toInt)
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

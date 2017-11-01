import spray.json.{DefaultJsonProtocol, DeserializationException, JsNumber, JsObject, JsString, JsValue, JsonFormat, RootJsonFormat}

case class Player(position: Vertex, angle: Int, id: Int)
case class MoveTestResponse(id: Int, x: Int, y: Int, result: Boolean)
case class Door(id: Int, distance: Double, state: String)
case class Object(position: Vertex, typeId: Int, distance: Double, health: Int, id: Int)
case class LosResponse(los: Boolean, id: Int, id2: Int)

object Protocols extends DefaultJsonProtocol {
  implicit val vertexFormat: JsonFormat[Vertex] = lazyFormat(jsonFormat(Vertex, "x", "y"))

  implicit object PlayerJsonFormat extends RootJsonFormat[Player] {
    override def write(obj: Player): JsValue = throw new RuntimeException("Writing Player objects Not supported")

    override def read(json: JsValue): Player = {
      json.asJsObject.getFields("position", "angle", "id") match {
        case Seq(JsObject(position), JsNumber(angle), JsNumber(id)) =>
          Player(position.toJson.convertTo[Vertex], angle.toInt, id.toInt)
        case _ => throw DeserializationException("Couldn't deserialize player")
      }
    }
  }

  implicit object ObjectJsonFormat extends RootJsonFormat[Object] {
    override def write(obj: Object): JsValue = throw new RuntimeException("Writing Objects Not supported")

    override def read(json: JsValue): Object = {
      json.asJsObject.getFields("position", "typeId", "distance", "health", "id") match {
        case Seq(JsObject(position), JsNumber(typeId), JsNumber(distance), JsNumber(health), JsNumber(id)) =>
          Object(position.toJson.convertTo[Vertex], typeId.toInt, distance.toDouble, health.toInt, id.toInt)
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

  implicit object DoorJsonFormat extends RootJsonFormat[Door] {
    override def write(obj: Door): JsValue = throw new RuntimeException("Writing Door objects not supported")

    override def read(json: JsValue): Door = {
      json.asJsObject.getFields("id", "distance", "state") match {
        case Seq(JsNumber(id), JsNumber(distance), JsString(state)) =>
          Door(id.toInt, distance.toDouble, state.toString)
        case _ => throw DeserializationException("Couldn't deserialize door")
      }
    }
  }

  implicit val MoveTestResponseFormat: JsonFormat[MoveTestResponse] = jsonFormat4(MoveTestResponse)
  implicit val LosResponseFormat: JsonFormat[LosResponse] = jsonFormat3(LosResponse)
}

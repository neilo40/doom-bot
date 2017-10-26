import spray.json.{DefaultJsonProtocol, DeserializationException, JsNumber, JsObject, JsValue, JsonFormat, RootJsonFormat}

case class Player(position: Vertex, angle: Int)

object Protocols extends DefaultJsonProtocol {
  implicit val vertexFormat: JsonFormat[Vertex] = lazyFormat(jsonFormat(Vertex, "x", "y"))

  implicit object PlayerJsonFormat extends RootJsonFormat[Player] {
    override def write(obj: Player): JsValue = throw new RuntimeException("Writing Player objects Not supported")

    override def read(json: JsValue): Player = {
      json.asJsObject.getFields("position", "angle") match {
        case Seq(JsObject(position), JsNumber(angle)) => Player(position.toJson.convertTo[Vertex], angle.toInt)
        case _ => throw DeserializationException("Couldn't deserialize player")
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
}

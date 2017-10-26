import scalaj.http.Http
import spray.json._
//import DefaultJsonProtocol._
import Protocols._

object PlayerController {

  def getPlayer: Player = {
    val playerJsonString = Http("http://localhost:6001/api/player").asString.body
    val jsonAst = playerJsonString.parseJson
    jsonAst.convertTo[Player]
  }
}

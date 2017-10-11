object BugFinder extends App{

  //val objects = scala.io.Source.fromURL("http://localhost:6001/api/world/objects").mkString
  //val doors = scala.io.Source.fromURL("http://localhost:6001/api/world/doors").mkString
  //val player = scala.io.Source.fromURL("http://localhost:6001/api/player").mkString
  //val players = scala.io.Source.fromURL("http://localhost:6001/api/players").mkString
  val world = scala.io.Source.fromURL("http://localhost:6001/api/world").mkString
  println(world)
}

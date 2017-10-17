import org.scalatest._

class TestWadLine extends FlatSpec with Matchers {

  "A Wadline" should "be able to be created" in {
    val wadLine = WadLine(Vertex(0, 0), Vertex(1, 1), oneSided = false)
    wadLine.a.y should be (0)
  }

  "Player movement" should "intersect with wall" in {
    val wall = WadLine(Vertex(1088, -3648), Vertex(1152, -3648), oneSided = true)
    val playerMove = WadLine(Vertex(1152, -3640), Vertex(1152, -3690), oneSided = false)
    val intersects = playerMove.intersectsWith(wall)
    intersects should be (true)
  }

}

import org.scalatest._

class TestWadLine extends FlatSpec with Matchers {

  "A Wadline" should "be able to be created" in {
    val wadLine = WadLine(Vertex(0, 0), Vertex(1, 1), nonTraversable = false)
    wadLine.a.y should be (0)
  }

  "Player movement" should "intersect with wall" in {
    val wall = WadLine(Vertex(1088, -3648), Vertex(1152, -3648), nonTraversable = true)
    val playerMove = WadLine(Vertex(1152, -3640), Vertex(1152, -3690), nonTraversable = false)
    val intersects = playerMove.intersectsWith(wall)
    intersects should be (true)
  }

  "The midpoint of Line (0, 0) -> (10, 10)" should "be (5, 5)" in {
    val line = WadLine(Vertex(0, 0), Vertex(10, 10), nonTraversable = false)
    line.midpoint should be (Vertex(5, 5))
  }
}

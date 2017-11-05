import org.scalatest._

class TestVertex extends FlatSpec with Matchers {

  "A Vertex" should "be able to be created" in {
    val vertex = Vertex(0, 0)
    vertex.x should be (0)
  }

  "A Vertex (0, 0)" should "be within 15 points of a Vertex (10, 10)" in {
    val a = Vertex(0, 0)
    val b = Vertex(10, 10)
    a.isCloseTo(b, 15) should be (true)
  }

  "A Vertex (-10, -10)" should "be within 15 points of a Vertex (0, 0)" in {
    val a = Vertex(-10, -10)
    val b = Vertex(0, 0)
    a.isCloseTo(b, 15) should be (true)
  }

  "A Vertex (0, 0)" should "not be within 10 points of a Vertex (-10, -10)" in {
    val a = Vertex(0, 0)
    val b = Vertex(-10, -10)
    a.isCloseTo(b, 10) should be (false)
  }
}


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import Main._

class MainTest extends FunSuite with ShouldMatchers {
  test("given 4 letter case") {
    Square.place(List(
      List("salt"),
      List("afar"),
      List("lava"),
      List("trap")
    )) should be(Seq(Square(Seq("salt", "afar", "lava", "trap"))))
  }

  test("given 4 letter case with herrings") {
    Square.place(List(
      List("salt", "asdf"),
      List("afar", "qewr"),
      List("lava", "zxcv"),
      List("trap", "pius")
    )) should be(Seq(Square(Seq("salt", "afar", "lava", "trap"))))
  }
}

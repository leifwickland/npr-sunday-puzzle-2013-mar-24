
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
    )).map(_.toWords) should be(Seq(Square.fromStrings(Seq("salt", "afar", "lava", "trap"))).map(_.toWords))
  }

  test("given 4 letter case with herrings") {
    Square.place(List(
      List("salt", "asdf"),
      List("afar", "qewr"),
      List("lava", "zxcv"),
      List("trap", "pius")
    )).head.toWords should be(Square.fromStrings(Seq("salt", "afar", "lava", "trap")).toWords)
  }
}

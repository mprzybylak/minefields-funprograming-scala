import org.scalatest.{Matchers, WordSpec}
import RecursiveIteration._

class RecursiveIterationTest extends WordSpec with Matchers {

  "factorial calculation" should {

    "calculate factorial for 0" in {
      factorial(0) shouldEqual 1
    }

    "calculate factorial for 1" in {
      factorial(1) shouldEqual 1
    }

    "calculate factorial for 2" in {
      factorial(2) shouldEqual 2
    }

    "calculate factorial for 3" in {
      factorial(3) shouldEqual 6
    }

    "calculate factorial for 7" in {
      factorial(7) shouldEqual 5040
    }
  }

}

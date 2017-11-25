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

  "fibonacci calculation" should {
    "calculate 1st fib = 0" in {
      fibonacci(0) shouldEqual 0
    }

    "calculate 2nd fib = 1" in {
      fibonacci(1) shouldEqual 1
    }

    "calculate 3rd fib = 1" in {
      fibonacci(2) shouldEqual 1
    }

    "calculate 4th fib = 2" in {
      fibonacci(3) shouldEqual 2
    }

    "calculate 5th fib = 3" in {
      fibonacci(4) shouldEqual 3
    }

    "calculate 6th fib = 5" in {
      fibonacci(5) shouldEqual 5
    }
  }
}

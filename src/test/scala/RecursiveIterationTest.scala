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

  "array sort checking" should {

    "confirm that sorted array is sorted" in {
      isSorted[Int](Array(1, 2, 3), (x, y) => x < y) shouldEqual true
    }

    "show that unordered array is not sorted" in {
      isSorted[Int](Array(1, 3, 2), (x, y) => x < y) shouldEqual false
    }

    "show that reverse-sorted array is not sorted" in {
      isSorted[Int](Array(3, 2, 1), (x, y) => x < y) shouldEqual false
    }

    "show that reverse-sorted array is sorted using reverse-sorted predicate" in {
      isSorted[Int](Array(3, 2, 1), (x, y) => x > y) shouldEqual true
    }
  }

  "partial application" should {

    "turn adding into increment" in {

      // given
      def add(a: Int, b: Int): Int = a + b
      val standardPartial = add(1, _: Int)

      // when
      val customPartial = partialApplication(1, add)

      // then
      standardPartial(3) shouldEqual customPartial(3)
    }

    "turn adding into increment with currying" in {

      // given
      def add(a: Int, b: Int): Int = a + b
      val standardPartial = add(1, _: Int)

      // when
      val curryingPartial = currying(add)(1)

      // then
      standardPartial(6) shouldEqual curryingPartial(6)
    }

    "turn curried adding into regular adding with uncurring" in {

      // given
      def add(a: Int, b: Int): Int = a + b
      val addCurry = (a: Int) => (b: Int) => a + b

      // when
      val addUncurried = uncurrying(addCurry)

      // then
      add(1, 2) shouldEqual addUncurried(1, 2)
    }
  }

  "composition" should {

    "compose two functions" in {

      // given
      def identity(a: Int) = a
      def inc(a: Int) = a + 1

      // when
      val composed = compose(identity, inc)

      // then
      composed(1) shouldEqual inc(identity(1))
    }
  }
}
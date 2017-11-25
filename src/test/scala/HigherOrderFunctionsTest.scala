import org.scalatest.{Matchers, WordSpec}

class HigherOrderFunctionsTest extends WordSpec with Matchers {

  import HigherOrderFunctions._

  "partial application" should {

    "turn adding into increment" in {

      // given
      def add(a: Int, b: Int): Int = a + b
      val standardPartial = add(1, _: Int) // this is scala syntax for partial application

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
      def addCurry(a: Int)(b: Int) = a + b // this is how we define curryied function in scala

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

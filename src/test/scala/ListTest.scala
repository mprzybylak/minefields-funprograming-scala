import org.scalatest.{Matchers, WordSpec}

class ListTest extends WordSpec with Matchers {

  import List._

  "list" should {

    "be empty if not contains any elements" in {

      // given
      val list: List[Int] = Nil

      // then
      list shouldEqual Nil
    }

    "contains value if have one element" in {

      // given
      val list: List[Int] = Cons(1, Nil)

      // then
      list shouldEqual Cons(1, Nil)
    }

    "allow to contains many elements" in {

      // given
      val list: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))

      // then
      list shouldEqual Cons(1, Cons(2, Cons(3, Nil)))
    }
  }

  "list of integers" should {

    "allows to calculate sum of empty list" in {

      // given
      val list = Nil

      // when
      val sumOfElements = sum(list)

      // then
      sumOfElements shouldEqual 0
    }

    "allows to calculate sum of one element list" in {

      // given
      val list = Cons(1, Nil)

      // when
      val sumOfElements = sum(list)

      // then
      sumOfElements shouldEqual 1
    }

    "allows to calculate sum of multiple elements in list" in {

      // given
      val list = Cons(1, Cons(2, Cons(3, Nil)))

      // when
      val sumOfElements = sum(list)

      // then
      sumOfElements shouldEqual 6
    }
  }
}

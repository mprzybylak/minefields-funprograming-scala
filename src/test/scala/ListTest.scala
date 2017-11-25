import org.scalatest.{Matchers, WordSpec}

class ListTest extends WordSpec with Matchers {

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
}

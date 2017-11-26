import org.scalatest.{Matchers, WordSpec}

class ListTest extends WordSpec with Matchers {

  import List._

  "list" should {

    "be empty if not contains any elements" in {

      // when
      val list = Nil

      // then
      list shouldEqual Nil
    }

    "contains value if have one element" in {

      // when
      val list = Cons(1, Nil)

      // then
      list shouldEqual Cons(1, Nil)
    }

    "allow to contains many elements" in {

      // given
      val list = Cons(1, Cons(2, Cons(3, Nil)))

      // then
      list shouldEqual Cons(1, Cons(2, Cons(3, Nil)))
    }

    "allows to construct itself from one element" in {

      // when
      val list = List(1)

      // then
      list shouldEqual Cons(1, Nil)
    }

    "allows to construct itself from many elements" in {

      // when
      val list = List(1, 2, 3)

      // then
      list shouldEqual Cons(1, Cons(2, Cons(3, Nil)))
    }

    "allows to return tail" in {

      // given
      val list = List(1, 2, 3)

      // when
      val tailOfList = tail(list)

      // then
      tailOfList shouldEqual List(2, 3)

    }

    "allows to return tail of empty list" in {

      // given
      val list = Nil

      // when
      val tailOfList = tail(list)

      // then
      tailOfList shouldEqual Nil
    }

    "allows to change head of empty list" in {

      // given
      val list = Nil

      // when
      val listWithHead = setHead(list, 1)

      // then
      listWithHead shouldEqual Cons(1, Nil)
    }

    "allows to change head of non-empty list" in {

      // given
      val list = List(1, 2, 3)

      // when
      val listWithNewHead = setHead(list, 10)

      // then
      listWithNewHead shouldEqual List(10, 2, 3)
    }

    "allows to drop items from empty list" in {

      // given
      val list = Nil

      // when
      val droppedList = drop(list, 1)

      // then
      droppedList shouldEqual Nil
    }

    "allows to drop items from empty list with predicate" in {

      // given
      val list = Nil

      // when
      val droppedList = dropWhile(list)(e => false)

      // then
      droppedList shouldEqual Nil
    }

    "allows to drop one element from list with one element" in {

      // given
      val list = List(1)

      // when
      val droppedList = drop(list, 1)

      // then
      droppedList shouldEqual Nil
    }

    "allows to drop one element from list with one element with predicate" in {

      // given
      val list = List(1)

      // when
      val droppedList = dropWhile(list)(e => true)

      // then
      droppedList shouldEqual Nil
    }

    "not drop one element from list with one element with predicate if predicate did not match element" in {

      // given
      val list = List(1)

      // when
      val droppedList = dropWhile(list)(e => false)

      // then
      droppedList shouldEqual List(1)
    }

    "allows to drop more elements than list contains" in {

      // given
      val list = List(1)

      // when
      val droppedList = drop(list, 10)

      // then
      droppedList shouldEqual Nil
    }

    "allows to drop elements from list with multiple elements" in {

      // given
      val list = List(1, 2, 3)

      // when
      val droppedList = drop(list, 1)

      // then
      droppedList shouldEqual List(2, 3)
    }

    "allows to drop elements from list with multiple elements with predicate" in {

      // given
      val list = List(1, 2, 3)

      // when
      val droppedList = dropWhile(list)(e => e == 1)

      // then
      droppedList shouldEqual List(2, 3)
    }

    "allows to drop many elements from list with multiple elements" in {

      // given
      val list = List(1, 2, 3, 4, 5)

      // when
      val droppedList = drop(list, 3)

      // then
      droppedList shouldEqual List(4, 5)
    }

    "allows to drop many elements from list with multiple elements with predicate" in {

      // given
      val list = List(1, 2, 3, 4, 5)

      // when
      val droppedList = dropWhile(list)(e => e < 4)

      // then
      droppedList shouldEqual List(4, 5)
    }

    "allows to append empty list to empty list" in {

      // given
      val first = List()
      val second = List()

      // when
      val both = append(first, second)

      // then
      both shouldEqual Nil
    }

    "allows to append list to empty list" in {

      // given
      val first = List()
      val second = List(1, 2, 3)

      // when
      val both = append(first, second)

      // then
      both shouldEqual List(1, 2, 3)
    }

    "allows to append empty list to list" in {

      // given
      val first = List(1, 2, 3)
      val second = List()

      // when
      val both = append(first, second)

      // then
      both shouldEqual List(1, 2, 3)
    }

    "allows to append list to list" in {

      // given
      val first = List(1, 2, 3)
      val second = List(4, 5, 6)

      // when
      val both = append(first, second)

      // then
      both shouldEqual List(1, 2, 3, 4, 5, 6)
    }

    "allows to drop last element of empty list" in {

      // given
      val list = List()

      // when
      val listWithoutLast = init(list)

      // then
      listWithoutLast shouldEqual Nil
    }

    "allows to drop last element from single element list" in {

      // given
      val list = List(1)

      // when
      val listWithoutLast = init(list)

      // then
      listWithoutLast shouldEqual Nil
    }

    "returns length of empty list" in {

      // given
      val list = Nil

      // when
      val len = listLength(list)

      // then
      len shouldEqual 0
    }

    "returns length of single element list" in {

      // given
      val list = List(1)

      // when
      val len = listLength(list)

      // then
      len shouldEqual 1
    }

    "returns length of multiple element list" in {

      // given
      val list = List(1,2,3)

      // when
      val len = listLength(list)

      // then
      len shouldEqual 3
    }
  }

  "fold right function" should {

    "allows to calculate sum of empty list using fold" in {

      // given
      val list: List[Int] = Nil

      // when
      val sumOfElements = foldRight(list, 0)(_ + _)

      // then
      sumOfElements shouldEqual 0
    }

    "allows to calculate sum of one element list using fold" in {

      // given
      val list = Cons(1, Nil)

      // when
      val sumOfElements = foldRight(list, 0)(_ + _)

      // then
      sumOfElements shouldEqual 1
    }

    "allows to calculate sum of multiple elements in list" in {

      // given
      val list = Cons(1, Cons(2, Cons(3, Nil)))

      // when
      val sumOfElements = foldRight(list, 0)(_ + _)

      // then
      sumOfElements shouldEqual 6
    }

    "allows to calculate product of empty list" in {

      // given
      val list: List[Int] = Nil

      // when
      val productOfElements = foldRight(list, 1)(_ * _)

      // then
      productOfElements shouldEqual 1
    }

    "allows to calculate product of one elemnt list" in {

      // given
      val list = Cons(2, Nil)

      // when
      val productOfElements = foldRight(list, 1)(_ * _)

      // then
      productOfElements shouldEqual 2
    }

    "allows to calculate product of multiple elements in list" in {

      // given
      val list = Cons(2, Cons(4, Cons(6, Nil)))

      // when
      val productOfElements = foldRight(list, 1)(_ * _)

      // then
      productOfElements shouldEqual 48
    }

    "allows to rebuild list" in {

      // given
      val list = List(1,2,3)
      val z:List[Int] = Nil

      // when
      val rebuildedList = foldRight(list, z)(Cons(_,_))

      // then
      rebuildedList shouldEqual list
    }
  }

  "fold left" should {

    "allows to calculate sum of empty list using fold" in {

      // given
      val list: List[Int] = Nil

      // when
      val sumOfElements = foldLeft(list, 0)(_ + _)

      // then
      sumOfElements shouldEqual 0
    }

    "allows to calculate sum of one element list using fold" in {

      // given
      val list = Cons(1, Nil)

      // when
      val sumOfElements = foldLeft(list, 0)(_ + _)

      // then
      sumOfElements shouldEqual 1
    }

    "allows to calculate sum of multiple elements in list" in  {

      // given
      val list = Cons(1, Cons(2, Cons(3, Nil)))

      // when
      val sumOfElements = foldLeft(list, 0)(_ + _)

      // then
      sumOfElements shouldEqual 6
    }

    "allows to calculate product of empty list" in {

      // given
      val list: List[Int] = Nil

      // when
      val productOfElements = foldLeft(list, 1)(_ * _)

      // then
      productOfElements shouldEqual 1
    }

    "allows to calculate product of one elemnt list" in {

      // given
      val list = Cons(2, Nil)

      // when
      val productOfElements = foldLeft(list, 1)(_ * _)

      // then
      productOfElements shouldEqual 2
    }

    "allows to calculate product of multiple elements in list" in {

      // given
      val list = Cons(2, Cons(4, Cons(6, Nil)))

      // when
      val productOfElements = foldLeft(list, 1)(_ * _)

      // then
      productOfElements shouldEqual 48
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

    "allows to calculate product of empty list" in {

      // given
      val list = Nil

      // when
      val productOfElements = product(list)

      // then
      productOfElements shouldEqual 1
    }

    "allows to calculate product of one elemnt list" in {

      // given
      val list = Cons(2, Nil)

      // when
      val productOfElements = product(list)

      // then
      productOfElements shouldEqual 2
    }

    "allows to calculate product of multiple elements in list" in {

      // given
      val list = Cons(2, Cons(4, Cons(6, Nil)))

      // when
      val productOfElements = product(list)

      // then
      productOfElements shouldEqual 48
    }
  }
}

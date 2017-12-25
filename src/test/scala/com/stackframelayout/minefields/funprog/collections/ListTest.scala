package com.stackframelayout.minefields.funprog.collections

import com.stackframelayout.minefields.funprog.collections
import org.scalatest.{Matchers, WordSpec}

class ListTest extends WordSpec with Matchers {

  import com.stackframelayout.minefields.funprog.collections.List._

  "list" should {

    "be empty if not contains any elements" in {

      // when
      val list = collections.Nil

      // then
      list shouldEqual collections.Nil
    }

    "contains value if have one element" in {

      // when
      val list = Cons(1, collections.Nil)

      // then
      list shouldEqual collections.Cons(1, collections.Nil)
    }

    "allow to contains many elements" in {

      // given
      val list = Cons(1, Cons(2, collections.Cons(3, collections.Nil)))

      // then
      list shouldEqual Cons(1, Cons(2, collections.Cons(3, collections.Nil)))
    }

    "allows to construct itself from one element" in {

      // when
      val list = collections.List(1)

      // then
      list shouldEqual collections.Cons(1, collections.Nil)
    }

    "allows to construct itself from many elements" in {

      // when
      val list = collections.List(1, 2, 3)

      // then
      list shouldEqual Cons(1, Cons(2, collections.Cons(3, collections.Nil)))
    }

    "allows to return tail" in {

      // given
      val list = collections.List(1, 2, 3)

      // when
      val tailOfList = tail(list)

      // then
      tailOfList shouldEqual collections.List(2, 3)

    }

    "allows to return tail of empty list" in {

      // given
      val list = collections.Nil

      // when
      val tailOfList = tail(list)

      // then
      tailOfList shouldEqual collections.Nil
    }

    "allows to change head of empty list" in {

      // given
      val list = collections.Nil

      // when
      val listWithHead = setHead(list, 1)

      // then
      listWithHead shouldEqual collections.Cons(1, collections.Nil)
    }

    "allows to change head of non-empty list" in {

      // given
      val list = collections.List(1, 2, 3)

      // when
      val listWithNewHead = setHead(list, 10)

      // then
      listWithNewHead shouldEqual collections.List(10, 2, 3)
    }

    "allows to drop items from empty list" in {

      // given
      val list = collections.Nil

      // when
      val droppedList = drop(list, 1)

      // then
      droppedList shouldEqual collections.Nil
    }

    "allows to drop items from empty list with predicate" in {

      // given
      val list = collections.Nil

      // when
      val droppedList = dropWhile(list)(e => false)

      // then
      droppedList shouldEqual collections.Nil
    }

    "allows to drop one element from list with one element" in {

      // given
      val list = collections.List(1)

      // when
      val droppedList = drop(list, 1)

      // then
      droppedList shouldEqual collections.Nil
    }

    "allows to drop one element from list with one element with predicate" in {

      // given
      val list = collections.List(1)

      // when
      val droppedList = dropWhile(list)(e => true)

      // then
      droppedList shouldEqual collections.Nil
    }

    "not drop one element from list with one element with predicate if predicate did not match element" in {

      // given
      val list = collections.List(1)

      // when
      val droppedList = dropWhile(list)(e => false)

      // then
      droppedList shouldEqual collections.List(1)
    }

    "allows to drop more elements than list contains" in {

      // given
      val list = collections.List(1)

      // when
      val droppedList = drop(list, 10)

      // then
      droppedList shouldEqual collections.Nil
    }

    "allows to drop elements from list with multiple elements" in {

      // given
      val list = collections.List(1, 2, 3)

      // when
      val droppedList = drop(list, 1)

      // then
      droppedList shouldEqual collections.List(2, 3)
    }

    "allows to drop elements from list with multiple elements with predicate" in {

      // given
      val list = collections.List(1, 2, 3)

      // when
      val droppedList = dropWhile(list)(e => e == 1)

      // then
      droppedList shouldEqual collections.List(2, 3)
    }

    "allows to drop many elements from list with multiple elements" in {

      // given
      val list = collections.List(1, 2, 3, 4, 5)

      // when
      val droppedList = drop(list, 3)

      // then
      droppedList shouldEqual collections.List(4, 5)
    }

    "allows to drop many elements from list with multiple elements with predicate" in {

      // given
      val list = collections.List(1, 2, 3, 4, 5)

      // when
      val droppedList = dropWhile(list)(e => e < 4)

      // then
      droppedList shouldEqual collections.List(4, 5)
    }

    "allows to append empty list to empty list" in {

      // given
      val first = collections.List()
      val second = collections.List()

      // when
      val both = append(first, second)

      // then
      both shouldEqual collections.Nil
    }

    "allows to append empty list to empty list with fold" in {

      // given
      val first = collections.List()
      val second = collections.List()

      // when
      val both = appendFold(first, second)

      // then
      both shouldEqual collections.Nil
    }

    "allows to append list to empty list" in {

      // given
      val first = collections.List()
      val second = collections.List(1, 2, 3)

      // when
      val both = append(first, second)

      // then
      both shouldEqual collections.List(1, 2, 3)
    }

    "allow sto append list to empty list with fold" in {

      // given
      val first = collections.List()
      val second = collections.List(1, 2, 3)

      // when
      val both = appendFold(first, second)

      // then
      both shouldEqual collections.List(1, 2, 3)
    }

    "allows to append empty list to list" in {

      // given
      val first = collections.List(1, 2, 3)
      val second = collections.List()

      // when
      val both = append(first, second)

      // then
      both shouldEqual collections.List(1, 2, 3)
    }

    "allows to append empty list to list with fold" in {

      // given
      val first = collections.List(1, 2, 3)
      val second = collections.List()

      // when
      val both = appendFold(first, second)

      // then
      both shouldEqual collections.List(1, 2, 3)
    }

    "allows to append list to list" in {

      // given
      val first = collections.List(1, 2, 3)
      val second = collections.List(4, 5, 6)

      // when
      val both = append(first, second)

      // then
      both shouldEqual collections.List(1, 2, 3, 4, 5, 6)
    }

    "allows to concat list of list for Nil list" in {

      // given
      val list = collections.Nil

      // when
      val c = concat(list)

      // then
      c shouldEqual collections.Nil
    }

    "allows to concat list of list with single list" in {

      // given
      val list = collections.List(collections.List(1, 2, 3))

      // when
      val c = concat(list)

      // then
      c shouldEqual collections.List(1, 2, 3)
    }

    "allows to concat list of list with two lists" in {

      // given
      val list = collections.List(collections.List(1, 2, 3), collections.List(4, 5, 6))

      // when
      val c = concat(list)

      // then
      c shouldEqual collections.List(1, 2, 3, 4, 5, 6)
    }

    "allows to concat list of list with multiple lists" in {

      // given
      val list = collections.List(collections.List(1, 2), collections.List(3, 4), collections.List(5, 6), collections.List(7, 8))

      // when
      val c = concat(list)

      // then
      c shouldEqual collections.List(1, 2, 3, 4, 5, 6, 7, 8)
    }

    "allows to drop last element of empty list" in {

      // given
      val list = collections.List()

      // when
      val listWithoutLast = init(list)

      // then
      listWithoutLast shouldEqual collections.Nil
    }

    "allows to drop last element from single element list" in {

      // given
      val list = collections.List(1)

      // when
      val listWithoutLast = init(list)

      // then
      listWithoutLast shouldEqual collections.Nil
    }

    "returns length of empty list" in {

      // given
      val list = collections.Nil

      // when
      val len = foldRightLength(list)

      // then
      len shouldEqual 0
    }

    "returns length of single element list" in {

      // given
      val list = collections.List(1)

      // when
      val len = foldRightLength(list)

      // then
      len shouldEqual 1
    }

    "returns length of multiple element list" in {

      // given
      val list = collections.List(1, 2, 3)

      // when
      val len = foldRightLength(list)

      // then
      len shouldEqual 3
    }
  }

  "fold right function" should {

    "allows to calculate sum of empty list using fold" in {

      // given
      val list: collections.List[Int] = collections.Nil

      // when
      val sumOfElements = foldRight(list, 0)(_ + _)

      // then
      sumOfElements shouldEqual 0
    }

    "allows to calculate sum of one element list using fold" in {

      // given
      val list = collections.Cons(1, collections.Nil)

      // when
      val sumOfElements = foldRight(list, 0)(_ + _)

      // then
      sumOfElements shouldEqual 1
    }

    "allows to calculate sum of multiple elements in list" in {

      // given
      val list = Cons(1, Cons(2, collections.Cons(3, collections.Nil)))

      // when
      val sumOfElements = foldRight(list, 0)(_ + _)

      // then
      sumOfElements shouldEqual 6
    }

    "allows to calculate product of empty list" in {

      // given
      val list: collections.List[Int] = collections.Nil

      // when
      val productOfElements = foldRight(list, 1)(_ * _)

      // then
      productOfElements shouldEqual 1
    }

    "allows to calculate product of one elemnt list" in {

      // given
      val list = collections.Cons(2, collections.Nil)

      // when
      val productOfElements = foldRight(list, 1)(_ * _)

      // then
      productOfElements shouldEqual 2
    }

    "allows to calculate product of multiple elements in list" in {

      // given
      val list = Cons(2, Cons(4, collections.Cons(6, collections.Nil)))

      // when
      val productOfElements = foldRight(list, 1)(_ * _)

      // then
      productOfElements shouldEqual 48
    }

    "allows to rebuild list" in {

      // given
      val list = collections.List(1, 2, 3)
      val z: collections.List[Int] = collections.Nil

      // when
      val rebuildedList = foldRight(list, z)(Cons(_, _))

      // then
      rebuildedList shouldEqual list
    }
  }

  "fold left" should {

    "allows to calculate sum of empty list using fold" in {

      // given
      val list: collections.List[Int] = collections.Nil

      // when
      val sumOfElements = foldLeft(list, 0)(_ + _)

      // then
      sumOfElements shouldEqual 0
    }

    "allows to calculate sum of one element list using fold" in {

      // given
      val list = collections.Cons(1, collections.Nil)

      // when
      val sumOfElements = foldLeft(list, 0)(_ + _)

      // then
      sumOfElements shouldEqual 1
    }

    "allows to calculate sum of multiple elements in list" in {

      // given
      val list = Cons(1, Cons(2, collections.Cons(3, collections.Nil)))

      // when
      val sumOfElements = foldLeft(list, 0)(_ + _)

      // then
      sumOfElements shouldEqual 6
    }

    "allows to calculate product of empty list" in {

      // given
      val list: collections.List[Int] = collections.Nil

      // when
      val productOfElements = foldLeft(list, 1)(_ * _)

      // then
      productOfElements shouldEqual 1
    }

    "allows to calculate product of one elemnt list" in {

      // given
      val list = collections.Cons(2, collections.Nil)

      // when
      val productOfElements = foldLeft(list, 1)(_ * _)

      // then
      productOfElements shouldEqual 2
    }

    "allows to calculate product of multiple elements in list" in {

      // given
      val list = Cons(2, Cons(4, collections.Cons(6, collections.Nil)))

      // when
      val productOfElements = foldLeft(list, 1)(_ * _)

      // then
      productOfElements shouldEqual 48
    }

    "returns length of empty list" in {

      // given
      val list = collections.Nil

      // when
      val len = foldLeftLength(list)

      // then
      len shouldEqual 0
    }

    "returns length of single element list" in {

      // given
      val list = collections.List(1)

      // when
      val len = foldLeftLength(list)

      // then
      len shouldEqual 1
    }

    "returns length of multiple element list" in {

      // given
      val list = collections.List(1, 2, 3)

      // when
      val len = foldLeftLength(list)

      // then
      len shouldEqual 3
    }

    "returns reverted list for empty list" in {

      // given
      val list = collections.List()

      // when
      val reverseList = reverseLeftFold(list)

      // then
      reverseList shouldEqual collections.Nil

    }

    "returns reverted list for single element list" in {

      // given
      val list = collections.List(1)

      // when
      val reverseList = reverseLeftFold(list)

      // then
      reverseList shouldEqual list
    }

    "returns reverted list for multiple elements list" in {

      // given
      val list = collections.List(1, 2, 3)

      // when
      val reverseList = reverseLeftFold(list)

      // then
      reverseList shouldEqual collections.List(3, 2, 1)
    }

    "allows to calculate sum of empty list using fold left in terms of fold right" in {

      // given
      val list: collections.List[Int] = collections.Nil

      // when
      val sumOfElements = foldLeftInTermsOfFoldRight(list, 0)(_ + _)

      // then
      sumOfElements shouldEqual 0
    }

    "allows to calculate sum of one element list using fold in terms of fold right" in {

      // given
      val list = collections.Cons(1, collections.Nil)

      // when
      val sumOfElements = foldLeftInTermsOfFoldRight(list, 0)(_ + _)

      // then
      sumOfElements shouldEqual 1
    }

    "allows to calculate sum of multiple elements in list in terms of fold right" in {

      // givenw
      val list = Cons(1, Cons(2, Cons(3, Cons(4, collections.Cons(5, collections.Nil)))))

      // when
      val sumOfElements = foldLeftInTermsOfFoldRight(list, 0)(_ + _)

      // then
      sumOfElements shouldEqual 15
    }


  }

  "list of integers" should {

    "allows to calculate sum of empty list" in {

      // given
      val list = collections.Nil

      // when
      val sumOfElements = sum(list)

      // then
      sumOfElements shouldEqual 0
    }

    "allows to calculate sum of one element list" in {

      // given
      val list = collections.Cons(1, collections.Nil)

      // when
      val sumOfElements = sum(list)

      // then
      sumOfElements shouldEqual 1
    }

    "allows to calculate sum of multiple elements in list" in {

      // given
      val list = Cons(1, Cons(2, collections.Cons(3, collections.Nil)))

      // when
      val sumOfElements = sum(list)

      // then
      sumOfElements shouldEqual 6
    }

    "allows to calculate product of empty list" in {

      // given
      val list = collections.Nil

      // when
      val productOfElements = product(list)

      // then
      productOfElements shouldEqual 1
    }

    "allows to calculate product of one elemnt list" in {

      // given
      val list = collections.Cons(2, collections.Nil)

      // when
      val productOfElements = product(list)

      // then
      productOfElements shouldEqual 2
    }

    "allows to calculate product of multiple elements in list" in {

      // given
      val list = Cons(2, Cons(4, collections.Cons(6, collections.Nil)))

      // when
      val productOfElements = product(list)

      // then
      productOfElements shouldEqual 48
    }

    "should allow to add 1 to empty list with fold" in {

      // given
      val list = collections.List()

      // when
      val listPlusOne = addOne(list)

      // when
      listPlusOne shouldEqual collections.Nil
    }

    "should allow to add 1 to non empty list with fold" in {

      // given
      val list = collections.List(1, 2, 3)

      // when
      val listPlusOne = addOne(list)

      // then
      listPlusOne shouldEqual collections.List(2, 3, 4)
    }

    "should change list of ints to list of strings for empty list with fold" in {

      // given
      val list = collections.List()

      // when
      val stringList = toStringList(list)

      // then
      stringList shouldEqual collections.Nil
    }

    "should change list of ints to list of strings for non-empty list with fold" in {

      // given
      val list = collections.List(1, 2, 3)

      // when
      val stringList = toStringList(list)

      // then
      stringList shouldEqual collections.List("1", "2", "3")
    }

    "should run function over empty lists" in {

      // given
      val list = collections.List()

      // when
      val mapList = map(list)(a => a.toString)

      // then
      mapList shouldEqual collections.Nil
    }

    "should run function over non-empty list" in {

      // given
      val list = collections.List(1,2,3)

      // when
      val mapList = map(list)(a => a * 2)

      // then
      mapList shouldEqual collections.List(2,4,6)
    }

    "should filter empty list" in {

      // given
      val list: collections.List[Int] = collections.List()

      // when
      val filterList = filter(list)(a => a % 2 == 0)

      // then
      filterList shouldEqual collections.Nil
    }

    "should filter odd elements" in {

      // given
      val list = collections.List(1,2,3)

      // when
      val filterList = filter(list)(a => a % 2 == 0)

      // then
      filterList shouldEqual collections.List(2)
    }

    "should flatMap empty list" in {

      // given
      val list:collections.List[Int] = collections.List()

      // when
      val flatMapList = flatMap(list)(a => collections.List(a * 2, a * 4))

      // then
      flatMapList shouldEqual collections.Nil
    }

    "should flatMap non empty list" in {

      // given
      val list:collections.List[Int] = collections.List(1,2,3)

      // when
      val flatMapList = flatMap(list)(a => collections.List(a*2, a*4))

      // then
      flatMapList shouldEqual collections.List(2,4, 4, 8, 6, 12)
    }

    "should filter empty list using flatMap" in {

      // given
      val list: collections.List[Int] = collections.List()

      // when
      val filterList = flatMapfilter(list)(a => a % 2 == 0)

      // then
      filterList shouldEqual collections.Nil
    }

    "should filter odd elements using flatMap" in {

      // given
      val list = collections.List(1,2,3)

      // when
      val filterList = flatMapfilter(list)(a => a % 2 == 0)

      // then
      filterList shouldEqual collections.List(2)
    }

    "should add coresponding elements for empty lists" in {

      // given
      val first = collections.List()
      val second = collections.List()

      // when
      val sum = sumList(first, second)

      // then
      sum shouldEqual collections.Nil
    }

    "should add corresponding elements for first empty and second non-empty list" in {

      // given
      val first = collections.List()
      val second = collections.List(1,2,3)

      // when
      val sum = sumList(first, second)

      // then
      sum shouldEqual collections.List(1,2,3)
    }

    "should add corresponding elements for first non-empty list and second empty list" in {

      // given
      val first = collections.List(1,2,3)
      val second = collections.List()

      // when
      val sum = sumList(first, second)

      // then
      sum shouldEqual collections.List(1,2,3)
    }

    "should add corresponding elements for two non empty list with the same number of elements" in {

      // given
      val first = collections.List(1,2,3)
      val second = collections.List(1,2,3)

      // when
      val sum = sumList(first, second)

      // then
      sum shouldEqual collections.List(2,4,6)
    }

    "should add corresponding elements for two non empty list where first is longer" in {

      // given
      val first = collections.List(1,2,3)
      val second = collections.List(1)

      //when
      val sum = sumList(first, second)

      // then
      sum shouldEqual collections.List(2,2,3)
    }

    // TODO finish list chapter
    "should add corresponding elements for two non empty listwhere second is longer" ignore {

      // given
      val first = collections.List(1)
      val second = collections.List(1,2,3)

      // when
      val sum = sumList(first, second)

      // then
      sum shouldEqual collections.List(2,2,3)
    }

    // TODO finish list chapter
    "should zip corresponding elements with given function for empty lists" ignore {

      // given
      val first = collections.List()
      val second = collections.List()

      // when
      val sum = zipWith(first, second)((a,b)=> "")

      // then
      sum shouldEqual collections.Nil
    }

    "should zip corresponding elements with given function for equaly long lists" in {

      // given
      val first = collections.List(1, 2, 3)
      val second = collections.List("a", "b", "c")

      // when
      val sum = zipWith(first, second)((a,b) => a.toString + b)
    }

  }
}

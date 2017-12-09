import scala.annotation.tailrec

/**
  * Sealed means that this class can be extend only inside this file
  *
  * +A in type declaration means that A is covariant, so
  * if Bar is subtype of Foo than List[Bar] is subtype of List[Foo]
  */
sealed trait List[+A]

/**
  * Case objects can take part in pattern matching
  *
  * Nothing is considered a subtype of any other type so it is valid to say:
  * val list: List[Int] = Nothing
  */
case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

/**
  * This is so called 'companion object' it have the same name as a type List
  * Companion objects should contain all logic that is related to class but does not
  * need instance of that class
  */
object List {

  def apply[A](elements: A*): List[A] = { // A* means variadic args function
    // _* operator means that collection (in this case sequence) should be passed as vararg
    if (elements.isEmpty) Nil else Cons(elements.head, List(elements.tail: _*))
  }

  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def product(xs: List[Int]): Int = xs match {
    case Nil => 1
    case Cons(head, tail) => head * product(tail)
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](xs: List[A], newHead: A): List[A] = xs match {
    case Nil => Cons(newHead, Nil)
    case Cons(_, t) => Cons(newHead, t)
  }

  def drop[A](xs: List[A], n: Int): List[A] = xs match {
    case Nil => Nil
    case Cons(_, t) => if (n > 1) drop(t, n - 1) else t
  }

  /**
    * Motivation behind having this function as curried:
    *
    * In case of non-curryied version our signature will look like this
    * dropWhile[A](xs: List[A], predicate: A => Boolean): List[A]
    *
    * example of execution (for list of ints):
    * dropWhile(list, (e:Int) => e > 5)
    *
    * scala is not able to infer type of e in passed predicate, but if we
    * use curryied version - type will be infered:
    * dropWhile(list)(e => e > 5)
    *
    * beacsue dropWhile(list) will return function with list of Int applyied
    * so it is not possible to have later predicate with type other than Int
    */
  def dropWhile[A](xs: List[A])(predicate: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case Cons(h, t) => if (predicate(h)) dropWhile(t)(predicate) else xs
  }

  def append[A](first: List[A], second: List[A]): List[A] = first match {
    case Nil => second
    case Cons(h, t) => Cons(h, append(t, second))
  }

  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(h, t) => if(tail(t) == Nil) Nil else Cons(h, init(t))
  }

  def foldRight[A,B](xs: List[A], z: B)(f: (A,B) => B): B = xs match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  @tailrec
  def foldLeft[A,B](xs: List[A], z: B)(f: (B, A) => B): B = xs match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z,h))(f)
  }

  def foldLeftInTermsOfFoldRight[A,B](xs: List[A], z: B)(f: (B, A) => B): B =
    xs match {
    case Nil => z
    case Cons(h, t) =>
      foldRight(t, f(z,h))((a,b)=>f(b,a))
    }

  def foldRightLength[A](as: List[A]): Int = foldRight(as, 0)((_, b) => b + 1)

  def foldLeftLength[A](as: List[A]): Int = foldLeft(as, 0)((a, _) => a + 1)

  def reverseLeftFold[A](as: List[A]): List[A] = foldLeft(as, Nil:List[A])((a,b) => Cons(b,a))

  def appendFold[A](first: List[A], second: List[A]): List[A] = {
    foldRight(first, second)((a,b) => Cons(a,b))
  }

  def concat[A](list: List[List[A]]): List[A] = foldRight(list, Nil:List[A])(append)

  def addOne(list: List[Int]): List[Int] = foldRight(list, Nil:List[Int])((a,b) => Cons(a+1, b))

  def toStringList(list: List[Int]): List[String] = foldRight(list, Nil:List[String])((a,b)=>Cons(a.toString, b))

}
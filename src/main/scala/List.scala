
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
    if(elements.isEmpty) Nil else Cons(elements.head, List(elements.tail: _*))
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

  def dropWhile[A](xs: List[A], predicate: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case Cons(h,t) => if(predicate(h)) dropWhile(t, predicate) else xs
  }

  def append[A](first: List[A], second: List[A]): List[A] = first match {
    case Nil => second
    case Cons(h,t) => Cons(h, append(t, second))
  }
}
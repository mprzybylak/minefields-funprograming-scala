
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

package com.stackframelayout.minefields.funprog

sealed trait Stream[+A] {
  def toList: List[A]
  def take(n: Int): Stream[A]
}


case object Empty extends Stream[Nothing] {
  override def toList: List[Nothing] = Nil
  override def take(n: Int): Stream[Nothing] = this
}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def toList: List[A] = this match {
    case c: Cons[A] => List(h()) ++ t().toList
  }

  override def take(n: Int): Stream[A] =
    if(n < 2) Cons(h, () => Empty) else  Cons(h, () => t().take(n-1))

}

object Stream {

  def emptyStream[A]: Stream[A] = Empty
  def consStream[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }


  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) emptyStream else consStream(as.head, apply(as.tail :_*))

  def headOption[A](s: Stream[A]): Option[A] = s match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }
}
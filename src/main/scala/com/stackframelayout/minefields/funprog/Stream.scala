package com.stackframelayout.minefields.funprog

sealed trait Stream[+A] {
  def headOption: Option[A]
  def toList: List[A]
  def take(n: Int): Stream[A]
  def drop(n: Int): Stream[A]
  def takeWhile(p: A => Boolean): Stream[A]
  def exists(p: A => Boolean): Boolean
  def foldRight[B](z: => B)(f: (A, => B) => B): B
}

case object Empty extends Stream[Nothing] {
  override def toList: List[Nothing] = Nil
  override def take(n: Int): Stream[Nothing] = this
  override def drop(n: Int): Stream[Nothing] = this
  override def takeWhile(p: Nothing => Boolean): Stream[Nothing] = this
  override def headOption: Option[Nothing] = None
  override def exists(p: Nothing => Boolean): Boolean = false
  override def foldRight[B](z: => B)(f: (Nothing, => B) => B): B = ???
}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def toList: List[A] = this match {
    case _: Cons[A] => List(h()) ++ t().toList
  }

  override def take(n: Int): Stream[A] =
    if(n < 2) Cons(h, () => Empty) else  Cons(h, () => t().take(n-1))

  override def drop(n: Int): Stream[A] =
    if(n < 1) Cons(h, t) else t().drop(n - 1)

  override def takeWhile(f: A => Boolean): Stream[A] =
    if(f(h())) Cons(h, () => t().takeWhile(f)) else Empty

  override def headOption = Some(h())

  override def exists(f: A => Boolean): Boolean =
    if(f(h())) true else t().exists(f)

  override def foldRight[B](z: => B)(f: (A, => B) => B): B = ???
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
}
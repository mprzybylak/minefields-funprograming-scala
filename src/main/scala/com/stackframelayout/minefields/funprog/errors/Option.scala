package com.stackframelayout.minefields.funprog.errors

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](default: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

case object None extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] = None

  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](default: => Option[B]): Option[B] = default

  override def filter(f: Nothing => Boolean): Option[Nothing] = None
}

case class Some[A](get: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = Some(f(get))

  override def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  override def getOrElse[B >: A](default: => B): B = get

  override def orElse[B >: A](default: => Option[B]): Option[B] = this

  override def filter(f: A => Boolean): Option[A] = if(f(get)) this else None
}

object Option {

  def mean(xs: Seq[Double]) : Option[Double] = xs match {
    case Seq() => None
    case s => Some(s.sum / s.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap(m => mean(xs map(x => math pow(x-m,2))))

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A,B,C](first: Option[A], second: Option[B])(f: (A,B) => C):Option[C] = {
    (first, second) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }
  }

  def sequence[A](xs: List[Option[A]]): Option[List[A]] =
    if(xs.contains(None)) None else Some(xs.map { case Some(a) => a })
}
package com.stackframelayout.minefields.funprog.errors

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >:E, B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: Nothing => B): Left[E] = this
  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Left[E] = this
  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b
  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Left[E] = this
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: A => B): Right[B] = Right(f(value))
  override def flatMap[EE >: Nothing, B](f: A => Either[EE, B]) = f(value)
  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Right[A] = this

  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for { v1 <- this; v2 <- b } yield f(v1, v2)
}
package com.stackframelayout.minefields.funprog.errors

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: Nothing => B): Left[E] = this
  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Left[E] = this
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: A => B): Right[B] = Right(f(value))
  override def flatMap[EE >: Nothing, B](f: A => Either[EE, B]) = f(value)
}
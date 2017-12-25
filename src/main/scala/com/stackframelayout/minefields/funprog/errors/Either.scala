package com.stackframelayout.minefields.funprog.errors

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
}

case class Left[+E](error: E) extends Either[E, Nothing] {
  override def map[B](f: Nothing => B): Left[E] = this
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: A => B): Right[B] = Right(f(value))
}
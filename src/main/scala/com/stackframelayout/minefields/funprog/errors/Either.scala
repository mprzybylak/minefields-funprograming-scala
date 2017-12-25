package com.stackframelayout.minefields.funprog.errors

sealed trait Either[+E, +A]
case class Left[+E](error: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
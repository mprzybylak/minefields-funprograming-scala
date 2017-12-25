package com.stackframelayout.minefields.funprog.functions

import scala.annotation.tailrec

object RecursiveIteration {

  def factorial(n: Int): Int = {

    @tailrec
    /**
      * this is a classic way of expressing looping algorithms
      * as recursion in scala - we will define internal function
      * that will take one additional argument - an accumulator
      * accumulator is used to pass information between the recursive
      * calls - in this case intermediate result of calculating factorial
      */
    def factorial(n: Int, acc: Int = 1): Int = {
      if (n <= 0) acc else factorial(n - 1, n * acc) // passing intermediate result
    }

    // starting computation
    factorial(n)
  }

  def fibonacci(n: Int): Int = {

    // adding @tailrec annotation forces compiler to fail if this is NOT tail-recursive method
    @tailrec
    def fib(n: Int, a: Int = 0, b: Int = 1): Int = {
      if (n < 1) a else fib(n - 1, a + b, a)
    }

    fib(n)
  }

  def isSorted[A](xa: Array[A], predicate: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(xa: Array[A], predicate: (A, A) => Boolean, n: Int = 0): Boolean = {
      if (xa.length - 1 <= n) true
      else if (predicate(xa(n), xa(n + 1))) loop(xa, predicate, n + 1)
      else false
    }

    loop(xa, predicate)
  }
}

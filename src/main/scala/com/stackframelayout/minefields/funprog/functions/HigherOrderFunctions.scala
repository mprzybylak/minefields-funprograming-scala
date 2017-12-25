package com.stackframelayout.minefields.funprog.functions

object HigherOrderFunctions {

  /**
    * Partial application allows to apply only one argument to the 2 argument function.
    * In result we will get a function that will take one argument and as a result will
    * call source function with both arguments
    */
  def partialApplication[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  /**
    *  Currying is a way of expressing n-argument function as a series of n 1-argument
    *  functions composition.
    *
    *  In this case we will put as an input 2 argument function, and as a result we will
    *  get one argument function that will return other one argument function that
    *  eventually will call source function
    */
  def currying[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => partialApplication(a, f)

  /**
    * Uncurrying is operation oposite to currying. So we will take function that takes
    * one argument and returns function that takes one argument and return result - and we
    * will build a function that will take two arguments and return result
    */
  def uncurrying[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  /**
    * Function composition is a operation that will take result of one function and pass
    * it as an argument to the second function. compose method will take two function
    * and will return one that will compose source ones
    */
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}

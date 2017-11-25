import scala.annotation.tailrec

object RecursiveIteration {

  def factorial(n: Int): Int = {
    @tailrec
    def factorial(n: Int, acc: Int = 1): Int = {
      if (n <= 0) acc else factorial(n - 1, n * acc)
    }

    factorial(n)
  }

  def fibonacci(n: Int): Int = {
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

  def partialApplication[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def currying[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => partialApplication(a, f)

  def uncurrying[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}

import scala.annotation.tailrec

object RecursiveIteration {

  def factorial(n: Int): Int = {
    @tailrec
    def factorial(n: Int, acc: Int = 1): Int = {
      if(n <= 0) acc else factorial(n-1, n * acc)
    }
    factorial(n)
  }

  def fibonacci(n: Int): Int = {
    @tailrec
    def fib(n: Int, a: Int = 0, b: Int = 1): Int = {
      if(n < 1) a else fib(n - 1, a + b, a)
    }
    fib(n)
  }

  def isSorted[A](xa: Array[A], predicate: (A, A) => Boolean): Boolean = {
    def loop[A](xa: Array[A], predicate: (A, A) => Boolean, n: Int = 0): Boolean = {
      if(xa.length - 1 <= n ) true
      else if(predicate(xa(n), xa(n + 1))) loop(xa, predicate, n + 1)
      else false
    }
    loop(xa, predicate)
  }
}

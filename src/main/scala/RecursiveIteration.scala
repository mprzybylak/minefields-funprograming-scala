import scala.annotation.tailrec

object RecursiveIteration {

  def factorial(n: Int): Int = {
    @tailrec
    def factorial(n: Int, acc: Int): Int = {
      if(n <= 0) acc else factorial(n-1, n * acc)
    }
    factorial(n, 1)
  }

  def fibonacci(n: Int): Int = {
    @tailrec
    def fib(n: Int, a: Int, b: Int): Int = {
      if(n < 1) a else fib(n - 1, a + b, a)
    }
    fib(n, 0, 1)
  }
}

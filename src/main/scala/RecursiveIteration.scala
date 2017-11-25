import scala.annotation.tailrec

object RecursiveIteration {

  def factorial(n: Int): Int = {
    @tailrec
    def factorial(n: Int, acc: Int): Int = {
      if(n <= 0) acc else factorial(n-1, n * acc)
    }
    factorial(n, 1)
  }
}

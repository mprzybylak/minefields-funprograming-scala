object RecursiveIteration {

  def factorial(n: Int): Int = {
    def factorial(n: Int, acc: Int): Int = {
      if(n <= 0) acc else factorial(n-1, n * acc)
    }
    factorial(n, 1)
  }
}

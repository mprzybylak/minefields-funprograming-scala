object HigherOrderFunctions {
  def partialApplication[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def currying[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => partialApplication(a, f)

  def uncurrying[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}

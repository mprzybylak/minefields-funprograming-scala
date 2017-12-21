sealed trait Option[+A] {
  def map[B](f: A=>B): Option[B]
}

case class Some[A](get: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = Some(f(get))
}

case object None extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] = None
}
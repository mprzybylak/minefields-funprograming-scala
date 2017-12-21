sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
}

case class Some[A](get: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = Some(f(get))

  override def flatMap[B](f: A => Option[B]): Option[B] = {
    f(get) match {
      case None => None
      case Some(v) => Some(v)
    }
  }
}

case object None extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] = None

  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None
}
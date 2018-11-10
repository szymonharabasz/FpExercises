package fpinscala.errorhandling

sealed trait Either[+E, +A] {
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(_) => this
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }
  def map[B](f: A => B): Either[E, B] = flatMap(a => Right(f(a)))
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C) = {
    this.flatMap((x1:A) => b.map(x2 => f(x1,x2)))
  }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

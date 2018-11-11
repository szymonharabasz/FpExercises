package fpinscala.errorhandling

import fpinscala.datastrcutures.{Nil, Cons, MyList}

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
    flatMap((x1: A) => b.map(x2 => f(x1, x2)))
  }
}

object Either {
  def traverse[E, A, B](as: MyList[A])(f: A => Either[E, B]): Either[E, MyList[B]] = as match {
    case Nil => Right(Nil:MyList[B])
    case Cons(a, t) => f(a) match {
      case Left(x) => Left(x)
      case Right(x) => traverse(t)(f) match {
        case Left(y) => Left(y)
        case Right(y) => Right(Cons(x,y))
      }
    }
  }
  def sequence[E, A](as: MyList[Either[E, A]]): Either[E, MyList[A]] = traverse(as)((x:Either[E, A]) => x)

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

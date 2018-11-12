package fpinscala.laziness

import fpinscala.datastrcutures.MyList
import fpinscala.datastrcutures.Nil

sealed trait Stream[+A] {
  def toList: MyList[A] = this match {
    case Empty => Nil
    case Cons(h, t) => fpinscala.datastrcutures.Cons(h(), t().toList)
  }
  def take(n:Int): Stream[A] = if (n == 0) Stream.empty else this match {
    case Empty => Empty
    case Cons(h, t) => Stream.cons(h(), t().take(n-1))
  }
  def drop(n:Int): Stream[A] = if (n == 0) this else this match {
    case Empty => Empty
    case Cons(_, t) => t().drop(n-1)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

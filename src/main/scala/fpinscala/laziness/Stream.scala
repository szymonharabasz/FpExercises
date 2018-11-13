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
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (p(h())) Stream.cons(h(), t().takeWhile(p)) else Empty
  }
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  def exists(p: A => Boolean): Boolean = foldRight(false)((a,b) => p(a) || b)
  def forAll(p: A => Boolean): Boolean = !exists(!p(_))
  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Stream.empty:Stream[A])(
    (a:A, b) => if (p(a)) Stream.cons(a,b) else Stream.empty)
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

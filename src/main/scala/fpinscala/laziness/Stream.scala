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
  def headOption: Option[A] = foldRight(None:Option[A])((a,b) => Some(a))
  def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty:Stream[A])(
    (a:A, b) => if (p(a)) Stream.cons(a,b) else b
  )
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty:Stream[B])(
    (a, b) => Stream.append(f(a),b)
  )
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

  def append[A](s1: Stream[A], s2: Stream[A]): Stream[A] = s1.foldRight(s2)(
    (a1,a2) => a2 match {
      case Empty => Stream.cons(a1,s2)
      case a => Stream.cons(a1,a)
    }
  )
}

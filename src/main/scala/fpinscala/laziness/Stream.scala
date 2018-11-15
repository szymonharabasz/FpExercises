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
  /*
  def foldLeft[B](z: B)(f: (B,A) => B): B = this match {
    case Cons(h, t) => t().foldLeft(f(z,h()))(f)
    case _ => z
  }
  def reverse[A]: Stream[A] = foldLeft(Empty:Stream[A])((b:Stream[A],a:A) => Cons(a,b))
  */
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
  def takeUnfold(n:Int): Stream[A] = this match {
    case Cons(h, _) => Stream.unfold(n)(n => if (n == 0) None else Some((h(), n - 1)))
    case _ => Empty
  }
  def mapUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Cons(h,t) => Some((f(h()),t()))
    case _ => None
  }
  def takeWhileUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) => if (p(h())) Some((h(), t())) else None
    case _ => None
  }
  def startsWith[A](s: Stream[A]): Boolean = Stream.unfold((this,s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some( ( h1() == h2(), (t1(), t2()) ) )
    case (Empty, Cons(_, _)) => Some( ( false, (Empty, Empty) ) )
    case _ => None
  }.forAll(a => a)
  def tails: Stream[Stream[A]] = Stream.unfold(this) {
    case Cons(h, t) => Some(Stream.cons(h(), t()), t())
    case _ => None
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

  def append[A](s1: Stream[A], s2: Stream[A]): Stream[A] = s1.foldRight(s2)(
    (a1,a2) => a2 match {
      case Empty => Stream.cons(a1,s2)
      case a => Stream.cons(a1,a)
    }
  )
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def from(n: Int): Stream[Int] = cons(n, from(n+1))
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((a,s)) => cons(a, unfold(s)(f))
    }
  }
  def constantUnfold[A](a: A): Stream[A] = unfold(1)(_ => Some((a,0)))
  def fromUnfold(n: Int): Stream[Int] = unfold(n)(a => Some(a,a+1))
  def onesUnfold(): Stream[Int] = constantUnfold(1)
  def fibsUnfold(): Stream[Int] = unfold((0,1))(s => Some(s._1,(s._2,s._1+s._2)))
  def zipWith[A](s1: Stream[A], s2: Stream[A])(f: (A,A) => A): Stream[A] = unfold(s1,s2){
    case (Cons(h1,t1), Cons(h2,t2)) => Some(( f(h1(),h2()), (t1(), t2()) ))
    case _ => None
  }
  def zipAll[A, B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold(s1,s2) {
    case (Cons(h1,t1), Cons(h2,t2)) => Some((Some(h1()),Some(h2())),(t1(),t2()))
    case (Cons(h1,t1), Empty) => Some((Some(h1()),None),(t1(),Empty))
    case (Empty, Cons(h1,t1)) => Some((None,Some(h1())),(Empty,t1()))
    case _ => None
  }
}

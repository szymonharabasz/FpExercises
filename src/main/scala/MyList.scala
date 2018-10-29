package fpinscala.datastrcutures

sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: MyList[A]): MyList[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: MyList[A], a: A): MyList[A] = l match {
    case Nil => Cons(a, Nil)
    case Cons(_, t) => Cons(a, t)
  }

  def drop[A](l: MyList[A], n: Int): MyList[A] = {
    @annotation.tailrec
    def loop(l: MyList[A], m: Int): MyList[A] = l match {
      case Nil => Nil
      case Cons(_, t) => if (m >= n) t else loop(t, m + 1)
    }

    loop(l, 1)
  }

  def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = {
    @annotation.tailrec
    def loop(l: MyList[A], m: Int): MyList[A] = l match {
      case Nil => Nil
      case Cons(h, t) => if (!f(h)) Cons(h, t) else loop(t, m + 1)
    }

    loop(l, 1)
  }

  def init[A](l: MyList[A]): MyList[A] = {
   // TODO: make it work
   // @annotation.tailrec
    def loop(l: MyList[A]): MyList[A] = l match {
      case Nil => Nil
      case Cons(a, Cons(_, Nil)) => Cons(a, Nil)
      case Cons(a, t) => Cons(a, loop(t))
    }

    loop(l)
  }

}
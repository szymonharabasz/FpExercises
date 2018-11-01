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

  def init[A](l: MyList[A]): MyList[A] = l match {
    case Nil => Nil
    case Cons(a, Cons(_, Nil)) => Cons(a, Nil)
    case Cons(a, t) => Cons(a, init(t))
  }

  def foldRight[A,B](as: MyList[A], z: B)(f: (A,B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A,B](as: MyList[A], z: B)(f: (B,A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
  }

  def length[A](as: MyList[A]): Int = foldRight(as, 0)((_,y) => 1 + y)

  def sumLeft[A](as: MyList[Int]) = foldLeft(as,0)(_+_)
  def productLeft[A](as: MyList[Int]) = foldLeft(as,1)(_*_)
  def lengthLeft[A](as: MyList[Int]) = foldLeft(as,0)((x,_) => x + 1)

  def reverse[A](as: MyList[A]): MyList[A] = foldLeft(as, Nil:MyList[A])((b:MyList[A],a:A) => Cons(a,b))

  def foldRightAsLeft[A,B](as: MyList[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(as),z)((a,b) => f(b,a))

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    foldRightAsLeft(a1,a2)((x,l) => l match {
      case Nil => Cons[A](x,a2)
      case l => Cons[A](x,l)
    })
}
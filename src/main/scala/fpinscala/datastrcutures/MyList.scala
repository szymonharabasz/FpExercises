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

  def sumLeft[A](as: MyList[Int]): Int = foldLeft(as,0)(_+_)
  def productLeft[A](as: MyList[Int]): Int = foldLeft(as,1)(_*_)
  def lengthLeft[A](as: MyList[Int]): Int = foldLeft(as,0)((x, _) => x + 1)

  def reverse[A](as: MyList[A]): MyList[A] = foldLeft(as, Nil:MyList[A])((b:MyList[A],a:A) => Cons(a,b))

  def foldRightAsLeft[A,B](as: MyList[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(as),z)((a,b) => f(b,a))

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    foldRightAsLeft(a1,a2)((x,l) => l match {
      case Nil => Cons[A](x,a2)
      case m => Cons[A](x,m)
    })

  def concat[A](as: MyList[MyList[A]]): MyList[A] = foldLeft(as, Nil:MyList[A])(append)

  def addOne(ns: MyList[Int]): MyList[Int] = ns match {
    case Nil => Nil
    case Cons(n, a) => Cons(n + 1, addOne(a))
  }

  def stringify(ds: MyList[Double]): MyList[String] = ds match {
    case Nil => Nil
    case Cons(d, a) => Cons(d.toString, stringify(a))
  }

  def map[A,B](as: MyList[A])(f: A => B): MyList[B] = as match {
    case Nil => Nil
    case Cons(a, t) => Cons(f(a), map(t)(f))
  }
  def addOne2(ns: MyList[Int]): MyList[Int] = map(ns)(_ + 1)
  def stringify2(ds: MyList[Double]): MyList[String] = map(ds)(_.toString())

  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] = as match {
    case Nil => Nil
    case Cons(a, t) => if (f(a)) Cons(a, filter(t)(f)) else filter(t)(f)
  }

  def filterEven(ns: MyList[Int]): MyList[Int] = filter(ns)(_ % 2 == 0)

  def flatMap[A,B](as: MyList[A])(f: A => MyList[B]): MyList[B] = as match {
    case Nil => Nil
    case Cons(a, t) => append(f(a), flatMap(t)(f))
  }

  def filter2[A](as: MyList[A])(f: A => Boolean): MyList[A] = flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)
  def filterEven2(ns: MyList[Int]): MyList[Int] = filter2(ns)(_ % 2 == 0)

  def addElements(a1: MyList[Int], a2: MyList[Int]): MyList[Int] = (a1, a2) match {
    case (_, Nil) | (Nil, _) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addElements(t1,t2))
  }

  def zipWith[A](a1: MyList[A], a2: MyList[A])(f: (A, A) => A): MyList[A] = (a1, a2) match {
    case (_, Nil) | (Nil, _) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  def addElements2(a1: MyList[Int], a2: MyList[Int]): MyList[Int] = zipWith(a1,a2)(_+_)

  def startsWith[A](sup: MyList[A], sub: MyList[A]): Boolean = (sup, sub) match {
    case (Nil, Nil) => true
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(h1, t1), Cons(h2, t2)) =>
      if (h1 != h2) false
      else startsWith(t1, t2)
  }

  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = sup match {
    case Nil => false
    case Cons(_, t) =>
      if (startsWith(sup, sub)) true
      else hasSubsequence(t, sub)
  }

  def fill[A](n: Int)(a: A): MyList[A] = {
    if (n == 0) Nil else Cons(a, fill(n-1)(a))
  }
}
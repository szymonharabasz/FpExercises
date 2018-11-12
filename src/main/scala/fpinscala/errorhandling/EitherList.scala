package fpinscala.errorhandling

import fpinscala.datastrcutures.{Cons, MyList, Nil}

sealed trait EitherList[+E, +A] {
  def map2[EE >: E, B, C](b: EitherList[EE, B])(f: (A, B) => C) = (this, b) match {
    case (_, NilList) => NilList
    case (NilList, _) => NilList
    case (RightList(a1), RightList(a2)) => RightList(f(a1,a2))
    case (LeftList(h1,t1), LeftList(h2,t2)) => EitherList.append(LeftList(h1,t1), LeftList(h2,t2))
    case (LeftList(h,t),_) => LeftList(h,t)
    case (_,LeftList(h,t)) => LeftList(h,t)
  }
}
case class RightList[+A](value: A) extends EitherList[Nothing, A]
case class LeftList[+E](head: E, tail: EitherList[E, Nothing]) extends EitherList[E, Nothing]
case object NilList extends EitherList[Nothing, Nothing]

object EitherList {
  def foldLeft[A,B](as: EitherList[A, Nothing], z: B)(f: (B,A) => B): B = as match {
    case NilList => z
    case RightList(_) => z
    case LeftList(x, xs) => foldLeft(xs, f(z,x))(f)
  }
  def foldRightAsLeft[A,B](as: EitherList[A, Nothing], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(as),z)((a,b) => f(b,a))
  def reverse[A](as: EitherList[A, Nothing]): EitherList[A, Nothing] =
    foldLeft(as, NilList:EitherList[A, Nothing])((b:EitherList[A, Nothing],a:A) => LeftList(a,b))
  def apply[E](as: E*): EitherList[E, Nothing] =
    if (as.isEmpty) NilList
    else LeftList(as.head, apply(as.tail: _*))
  def append[A](a1: EitherList[A, Nothing], a2: EitherList[A, Nothing]): EitherList[A, Nothing] =
    foldRightAsLeft(a1,a2)((x,l) => l match {
      case NilList => LeftList[A](x,a2)
      case m => LeftList[A](x,m)
    })
  def traverse[E, A, B](as: MyList[A])(f: A => EitherList[E, B]): EitherList[E, MyList[B]] = as match {
    case Nil => RightList(Nil:MyList[B])
    case Cons(a, t) => (f(a), traverse(t)(f)) match {
      case (LeftList(e1,t1), LeftList(e2,t2)) => EitherList.append(LeftList(e1,t1),LeftList(e2,t2))
      case (RightList(a1), RightList(a2)) => RightList(MyList.append(Cons(a1,Nil),a2))
      case (RightList(_), LeftList(e1, t1)) => LeftList(e1,t1)
      case (LeftList(e1, t1), RightList(_)) => LeftList(e1,t1)
      case (_, NilList) => NilList
      case (NilList, _) => NilList
    }
  }
}
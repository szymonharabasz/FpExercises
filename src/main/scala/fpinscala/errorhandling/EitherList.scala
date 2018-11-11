package fpinscala.errorhandling

sealed trait EitherList[+E, +A] {
  def foldLeft[A,B](as: EitherList[A, Nothing], z: B)(f: (B,A) => B): B = as match {
    case NilList => z
    case LeftList(x, xs) => foldLeft(xs, f(z,x))(f)
  }
  def foldRightAsLeft[A,B](as: EitherList[A, Nothing], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(as),z)((a,b) => f(b,a))
  def reverse[A](as: EitherList[A, Nothing]): EitherList[A, Nothing] =
    foldLeft(as, NilList:EitherList[A, Nothing])((b:EitherList[A, Nothing],a:A) => LeftList(a,b))
  def append[A](a1: EitherList[A, Nothing], a2: EitherList[A, Nothing]): EitherList[A, Nothing] =
    foldRightAsLeft(a1,a2)((x,l) => l match {
      case NilList => LeftList[A](x,a2)
      case m => LeftList[A](x,m)
    })

  def orElse[EE >: E, B >: A](b: => EitherList[EE, B]): EitherList[EE, B] = this match {
    case NilList => NilList
    case LeftList(_,_) => b
    case RightList(_) => this
  }
  def flatMap[EE >: E, B](f: A => EitherList[EE, B]): EitherList[EE, B] = this match {
    case NilList => NilList
    case RightList(a) => f(a)
    case LeftList(h,t) => append(EitherList(h), t.flatMap(f))
  }
  def map[B](f: A => B): EitherList[E, B] = flatMap(a => RightList(f(a)))
  def map2[EE >: E, B, C](b: EitherList[EE, B])(f: (A, B) => C) = {
    flatMap((x1: A) => b.map(x2 => f(x1, x2)))
  }
}
case class RightList[+A](value: A) extends EitherList[Nothing, A]
case class LeftList[+E](head: E, tail: EitherList[E, Nothing]) extends EitherList[E, Nothing]
case object NilList extends EitherList[Nothing, Nothing]

object EitherList {
  def apply[E](as: E*): EitherList[E, Nothing] =
    if (as.isEmpty) NilList
    else LeftList(as.head, apply(as.tail: _*))
}
package fpinscala.errorhandling

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import fpinscala.datastrcutures.{Cons, MyList, Nil}

import scala.{Either => _, Option => _, _}

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = flatMap(a => Some(f(a)))
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this map(Some(_)) getOrElse ob
  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap((x1:A) => b.map(x2 => f(x1,x2)))

  def sequence[A](a: MyList[Option[A]]): Option[MyList[A]] = a match {
    case Nil => Some(Nil:MyList[A])
    case Cons(a:Option[A], t) => Some(Cons(a.getOrElse(return None), sequence(t) getOrElse(return None)))
  }

  def traverse[A, B](a: MyList[A])(f: A => Option[B]): Option[MyList[B]] = a match {
    case Nil => Some(Nil:MyList[B])
    case Cons(a, t:MyList[A]) =>
      Some(Cons(f(a).getOrElse(return None), traverse(t)(f).getOrElse(return None)):MyList[B])
  }

  def sequence_trav[A](a: MyList[Option[A]]): Option[MyList[A]] = traverse(a)((x:Option[A]) => x)
}


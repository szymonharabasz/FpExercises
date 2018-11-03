package fpinscala.datastrcutures

sealed trait MyTree[+A]
case class Leaf[A](value: A) extends MyTree[A]
case class Branch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {
  def size[A](as: MyTree[A]): Int = as match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def max(ns: MyTree[Int]): Int = ns match {
    case Leaf(n) => n
    case Branch(l, r) => max(l) max max(r)
  }

  def depth[A](as: MyTree[A]): Int = as match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](as: MyTree[A])(f: A => B): MyTree[B] = as match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](as: MyTree[A])(f: A => B, g: (B, B) => B): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f,g), fold(r)(f,g))
  }

  def sizeFold[A](as: MyTree[A]): Int = fold(as)(_ => 1, (a: Int, b:Int) => a + b)
  def maxFold(ns: MyTree[Int]): Int = fold(ns)((a: Int) => a, (a: Int, b: Int) => a max b)
  def depthFold(as: MyTree[Int]): Int = fold(as)(_ => 1, (a: Int, b: Int) => 1 + (a max b))
  def mapFold[A,B](as: MyTree[A])(f: A => B): MyTree[B] =
    fold(as)(a => Leaf(f(a)), (a: MyTree[B], b: MyTree[B]) => Branch(a,b))
}
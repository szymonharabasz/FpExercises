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
}
package fpinscala.state

import fpinscala.datastrcutures.MyList
import fpinscala.datastrcutures.Cons
import fpinscala.datastrcutures.Nil

abstract class RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n1,rng1) = rng.nextInt
    if (n1 < 0) return nonNegativeInt(rng1)
    (n1, rng1)
  }
  def nextDouble(rng: RNG): (Double, RNG) = {
    val (n1, rng1) = nonNegativeInt(rng)
    if (n1 == Int.MaxValue) nextDouble(rng1)
    (n1.toDouble / -Int.MinValue.toDouble, rng1)
  }
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n1, rng1) = rng.nextInt
    val (d2, rng2) = nextDouble(rng1)
    ((n1,d2),rng2)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((n1, d1), rng1) = intDouble(rng)
    ((d1,n1),rng1)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = nextDouble(rng)
    val (d2, rng2) = nextDouble(rng1)
    val (d3, rng3) = nextDouble(rng2)
    ((d1,d2,d3),rng3)
  }
  def ints(count: Int)(rng: RNG): (MyList[Int], RNG) = {
    if (count <= 0) (Nil,rng) else {
      val (n1, rng1) = rng.nextInt
      val (t, rng2) = ints(count - 1)(rng1)
      (Cons[Int](n1, t), rng2)
    }
  }
  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = (a,_)
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng1) = s(rng)
    (f(a), rng1)
  }
  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)
  def nextDoubleViaMap: Rand[Double] = map(nonNegativeInt)(_.toDouble / -Int.MinValue.toDouble)
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a1, rng1) = ra(rng)
    val (a2, rng2) = rb(rng1)
    (f(a1,a2), rng2)
  }
  def sequence[A](fs: MyList[Rand[A]]): Rand[MyList[A]] = fs match {
    case Cons(h:Rand[A], t:MyList[Rand[A]]) => rnd => {
      val (ha, rnd1) = h(rnd)
      val (ta:MyList[A], rnd2) = sequence(t)(rnd1)
      (Cons(ha, ta),rnd2)
    }
    case _ => rnd => (Nil, rnd)
  }
  def intsViaSequence(count: Int)(rng: RNG): (MyList[Int], RNG) =
    sequence[Int](MyList.fill(count)(_.nextInt))(rng)
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rnd => {
    val (a, rnd1) = f(rnd)
    g(a)(rnd1)
  }
  
  def main(args: Array[String]) = {
    val r1 = new SimpleRNG(42)
    val (d2, r2) = nextDouble(r1)
    println(d2)
    val (d3, r3) = nextDouble(r2)
    println(d3)
    val (l4, r4) = ints(10)(r3)
    val (l5, r5) = intsViaSequence(10)(r3)
    println(l4)
    println(l5)
  }
}
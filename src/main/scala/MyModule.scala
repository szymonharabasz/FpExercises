import fpinscala.datastrcutures.{Nil, Cons, MyList, Leaf, Branch, MyTree}

object MyModule {

  // Exercise 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(m: Int, prevprev: Int, prev: Int): Int = {
      if (m  >= n + 1) prevprev + prev
      else go(m + 1, prev, prevprev + prev)
    }

    go(3, 0, 1)
  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (ordered(as(n),as(n-1))) false
      else loop(n+1)
    }

    loop(1)
  }

  // Exercise 2.3
  def curry[A,B,C](f: (A, B) => C): A => B => C = a => b => f(a, b)
  // Exercise 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
  // Exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

}
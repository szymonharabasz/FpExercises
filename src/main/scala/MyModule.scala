import fpinscala.datastrcutures.MyList

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
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a) => ((b) => f(a, b))
  // Exercise 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
  // Exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a) => f(g(a))

  def main(args: Array[String]): Unit = {
    println("Run exercises :")
    println("-> Exercise 2.1:")
    println("%d %d %d %d %d %d %d %d %d %d"
      .format(fib(1), fib(2), fib(3), fib(4), fib(5), fib(6), fib(7), fib(8), fib(9), fib(10)))

    println("-> Exercise 2.2:")
    println(isSorted(Array(2, 4, 6, 8), (x: Int, y: Int) => if (x <= y) true else false))
    println(isSorted(Array(2, 4, 8, 6), (x: Int, y: Int) => if (x <= y) true else false))

    println("-> Exercise 3.2:")
    println(MyList.tail(MyList(1, 2, 3, 4)))

    println("-> Exercise 3.3:")
    println(MyList.setHead(MyList(1, 2, 3, 4), 6))

    println("-> Exercise 3.4:")
    println(MyList.drop(MyList(1, 2, 3, 4), 2))
    println(MyList.drop(MyList(1, 2, 3, 4), 5))

    println("-> Exercise 3.5:")
    println(MyList.dropWhile(MyList(1, 2, 3, 4), (a: Int) => (a <= 3)))
    println(MyList.dropWhile(MyList(1, 2, 3, 4), (_: Int) => true))

    println("-> Exercise 3.6:")
    println(MyList.init(MyList(1, 2, 3, 4)))
  }
}
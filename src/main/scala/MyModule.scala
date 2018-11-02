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
    println(MyList.dropWhile(MyList(1, 2, 3, 4), (a: Int) => a <= 3))
    println(MyList.dropWhile(MyList(1, 2, 3, 4), (_: Int) => true))

    println("-> Exercise 3.6:")
    println(MyList.init(MyList(1, 2, 3, 4)))

    println("-> Exercise 3.8")
    println( MyList.foldRight(MyList(1,2,3), Nil:MyList[Int])(Cons(_,_)) )

    println("-> Exercise 3.9")
    println(MyList.length(MyList(1,2,3,4,5)))

    println("-> Exercise 3.11")
    println(MyList.sumLeft(MyList(1,2,3,4,5)))
    println(MyList.productLeft(MyList(1,2,3,4,5)))
    println(MyList.lengthLeft(MyList(1,2,3,4,5)))

    println("-> Exercise 3.12")
    println(MyList.reverse(MyList(1,2,3,4,5)))

    println("-> Exercise 3.13")
    println(MyList.foldLeft(MyList("1","2","3","4","5"),"")(_+_))
    println(MyList.foldRight(MyList("1","2","3","4","5"),"")(_+_))
    println(MyList.foldRightAsLeft(MyList("1","2","3","4","5"),"")(_+_))

    println("-> Exercise 3.14")
    println(MyList.append(MyList(1,2,3),MyList(4,5,6)))

    println("-> Exercise 3.15")
    println(MyList.concat(MyList(MyList(1,2,3),MyList(4,5,6),MyList(7,8))))

    println("-> Exercise 3.16")
    println(MyList.addOne(MyList(1,2,3)))

    println("-> Exercise 3.17")
    println(MyList.stringify(MyList(1.2,2.3,3.4)))

    println("-> Exercise 3.18")
    println(MyList.addOne(MyList(1,2,3)) == MyList.addOne2(MyList(1,2,3)))
    println(MyList.stringify(MyList(1.2,2.3,3.4)) == MyList.stringify2(MyList(1.2,2.3,3.4)))

    println("-> Exercise 3.19")
    println(MyList.filterEven(MyList(1,2,3,4,5)))

    println("-> Exercise 3.20")
    println(MyList.flatMap(MyList(1,2,3))(i => MyList(i,i)))

    println("-> Exercise 3.21")
    println(MyList.filterEven2(MyList(1,2,3,4,5)))

    println("-> Exercise 3.22")
    println(MyList.addElements(MyList(1,2,3), MyList(4,5,6,7)))

    println("-> Exercise 3.23")
    println(MyList.addElements2(MyList(1,2,3), MyList(4,5,6,7)) == MyList.addElements(MyList(1,2,3), MyList(4,5,6,7)))

    println("-> Exercise 3.25")
    println(MyTree.size(Branch(Branch(Leaf(3),Leaf(2)),Leaf(4))))

    println("-> Exercise 3.26")
    println(MyTree.max(Branch(Branch(Leaf(3),Leaf(4)),Leaf(3))))
  }
}
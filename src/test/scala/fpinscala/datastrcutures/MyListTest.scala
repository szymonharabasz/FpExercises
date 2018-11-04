package fpinscala.datastrcutures

import org.scalatest.FunSuite

class MyListTest extends FunSuite {
  test("-> Exercise 3.2") {
    assert(MyList.tail(MyList(1, 2, 3, 4)) == MyList(2, 3, 4))
  }

  test("-> Exercise 3.3:") {
    assert(MyList.setHead(MyList(1, 2, 3, 4), 6) == MyList(6, 2, 3, 4))
  }

  test("-> Exercise 3.4:") {
    assert(MyList.drop(MyList(1, 2, 3, 4), 2) == MyList(3, 4))
    assert(MyList.drop(MyList(1, 2, 3, 4), 5) == Nil)
  }

  test("-> Exercise 3.5:") {
    assert(MyList.dropWhile(MyList(1, 2, 3, 4), (a: Int) => a <= 3) == MyList(4))
    assert(MyList.dropWhile(MyList(1, 2, 3, 4), (_: Int) => true) == Nil)
  }

  test("-> Exercise 3.6:") {
    assert(MyList.init(MyList(1, 2, 3, 4)) == MyList(1, 2, 3))
  }

  test("-> Exercise 3.8") {
    assert(MyList.foldRight(MyList(1, 2, 3), Nil: MyList[Int])(Cons(_, _)) == MyList(1, 2, 3))
  }

  test("-> Exercise 3.9") {
    assert(MyList.length(MyList(1, 2, 3, 4, 5)) == 5)
  }

  test("-> Exercise 3.11") {
    assert(MyList.sumLeft(MyList(1, 2, 3, 4, 5)) == 15)
    assert(MyList.productLeft(MyList(1, 2, 3, 4, 5)) == 120)
    assert(MyList.lengthLeft(MyList(1, 2, 3, 4, 5)) == 5)
  }

  test("-> Exercise 3.12") {
    assert(MyList.reverse(MyList(1, 2, 3, 4, 5)) == MyList(5, 4, 3, 2, 1))
  }

  test("-> Exercise 3.13") {
    assert(MyList.foldLeft(MyList("1", "2", "3", "4", "5"), "")(_ + _) == "12345")
    assert(MyList.foldRight(MyList("1", "2", "3", "4", "5"), "")(_ + _) == "12345")
    assert(MyList.foldRightAsLeft(MyList("1", "2", "3", "4", "5"), "")(_ + _) == "12345")
  }

  test("-> Exercise 3.14") {
    assert(MyList.append(MyList(1, 2, 3), MyList(4, 5, 6)) == MyList(1, 2, 3, 4, 5, 6))
  }

  test("-> Exercise 3.15") {
    assert(MyList.concat(MyList(MyList(1, 2, 3), MyList(4, 5, 6), MyList(7, 8))) == MyList(1, 2, 3, 4, 5, 6, 7, 8))
  }

  test("-> Exercise 3.16") {
    assert(MyList.addOne(MyList(1, 2, 3)) == MyList(2, 3, 4))
  }

  test("-> Exercise 3.17") {
    assert(MyList.stringify(MyList(1.2, 2.3, 3.4)) == MyList("1.2", "2.3", "3.4"))
  }

  test("-> Exercise 3.18") {
    assert(MyList.addOne(MyList(1, 2, 3)) == MyList.addOne2(MyList(1, 2, 3)))
    assert(MyList.stringify(MyList(1.2, 2.3, 3.4)) == MyList.stringify2(MyList(1.2, 2.3, 3.4)))
  }

  test("-> Exercise 3.19") {
    assert(MyList.filterEven(MyList(1, 2, 3, 4, 5)) == MyList(2, 4))
  }

  test("-> Exercise 3.20") {
    assert(MyList.flatMap(MyList(1, 2, 3))(i => MyList(i, i)) == MyList(1, 1, 2, 2, 3, 3))
  }

  test("-> Exercise 3.21") {
    assert(MyList.filterEven2(MyList(1, 2, 3, 4, 5)) == MyList(2, 4))
  }

  test("-> Exercise 3.22") {
    assert(MyList.addElements(MyList(1, 2, 3), MyList(4, 5, 6, 7)) == MyList(5, 7, 9))
  }

  test("-> Exercise 3.23") {
    assert(MyList.addElements2(MyList(1, 2, 3), MyList(4, 5, 6, 7)) == MyList.addElements(MyList(1, 2, 3), MyList(4, 5, 6, 7)))
  }

  test("-> Exercise 3.24") {
    assert(MyList.hasSubsequence(MyList(1,2,3,4,5,6), MyList(2,3,4)))
    assert(!MyList.hasSubsequence(MyList(1,2,3,4,5,6), MyList(2,1,3)))
  }
}

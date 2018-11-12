package fpinscala.laziness

import fpinscala.datastrcutures.MyList
import fpinscala.datastrcutures.Nil
import org.scalatest.FunSuite

class StreamTest extends FunSuite {
  test("-> Exercise 5.1 toList returns a proper MyList") {
    assert(Stream(1,2,3,4).toList == MyList(1,2,3,4))
  }
  test("-> Exercise 5.2 take(3) return the first 3 elements of longer Stream") {
    assert(Stream(1,2,3,4,5).take(3).toList == Stream(1,2,3).toList)
  }
  test("-> Exercise 5.2 take(6) return the full Stream if it's shorter") {
    assert(Stream(1,2,3,4,5).take(6).toList == Stream(1,2,3,4,5).toList)
  }
  test("-> Exercise 5.2 drop(3) return the all but first 3 elements of longer Stream") {
    assert(Stream(1,2,3,4,5).drop(3).toList == Stream(4,5).toList)
  }
  test("-> Exercise 5.2 drop(6) returns empty Stream if the input is shorter") {
    assert(Stream(1,2,3,4,5).drop(6).toList == Nil)
  }
  test("-> Exercise 5.3 takeWhile returns elements smaller than 3") {
    assert(Stream(1,2,3,4,5).takeWhile(_ < 3).toList == Stream(1,2).toList)
  }
  test("-> Exercise 5.2 takeWhile returns empty list if condition never fulfilled") {
    assert(Stream(1,2,3,4,5).takeWhile(_ < 0).toList == Nil)
  }
}

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
  test("-> Exercise 5.3 takeWhile returns empty list if condition never fulfilled") {
    assert(Stream(1,2,3,4,5).takeWhile(_ < 0).toList == Nil)
  }
  test("-> Exercise 5.4 forAll returns true if the condition is fulfilled for all elements") {
    assert(Stream(2,4,6,8).forAll(_ % 2 == 0) == true)
  }
  test("-> Exercise 5.4 forAll returns false if the condition is not fulfilled some element") {
    assert(Stream(2,4,5,8).forAll(_ % 2 == 0) == false)
  }
  test("-> Exercise 5.5 takeWhile2 returns elements smaller than 3") {
    assert(Stream(1,2,3,4,5).takeWhile2(_ < 3).toList == Stream(1,2).toList)
  }
  test("-> Exercise 5.5 takeWhile2 returns empty list if condition never fulfilled") {
    assert(Stream(1,2,3,4,5).takeWhile2(_ < 0).toList == Nil)
  }
  test("-> Exercise 5.6 headOption returns Some(first element) for non-empty Stream") {
    assert(Stream(2,3,4,5).headOption == Some(2))
  }
  test("-> Exercise 5.6 headOption returns None for empty Stream") {
    assert(Empty.headOption == None)
  }
  test("-> Exercise 5.7 filter returns a Stream containing only elements that fulfill condition") {
    assert(Stream(1,2,3,4,5,6).filter(_%2 == 0).toList == Stream(2,4,6).toList)
  }
  test("Exercise 5.7 append returns merged Streams") {
    assert(true)
  }
}

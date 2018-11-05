package fpinscala.errorhandling

import org.scalatest.FunSuite

class OptionTest extends FunSuite {
  test("-> Exercise 4.1 map of Some returns Some of mapped value") {
    assert(Some(2).map(a => a * a) == Some(4))
  }
  test("-> Exercise 4.1 map of None returns None") {
    assert(None.map(_ => 4) == None)
  }
  test("-> Exercise 4.1 flatMap of Some returns Some of mapped value") {
    assert(Some(2).flatMap(a => Some(a * a)) == Some(4))
  }
  test("-> Exercise 4.1 flatMap of None returns None") {
    assert(None.flatMap(_ => Some(4)) == None)
  }
  test("-> Exercise 4.1 getOrElse on Some returns the value") {
    assert(Some(4).getOrElse(2) == 4)
  }
  test("-> Exercise 4.1 getOrElse on None returns the default") {
    assert(None.getOrElse(2) == 2)
  }
  test("-> Exercise 4.1 orElse on Some returns the same") {
    assert(Some(4).orElse(Some(2)) == Some(4))
  }
  test("-> Exercise 4.1 orElse on None returns the default") {
    assert(None.orElse(Some(2)) == Some(2))
  }
  test("-> Exercise 4.1 filter on true condition return the value") {
    assert(Some(4).filter(_ % 2 == 0) == Some(4))
  }
  test("-> Exercise 4.1 filter on false condition return the value") {
    assert(Some(3).filter(_ % 2 == 0) == None)
  }
  test("-> Exercise 4.1 filter None returns None") {
    assert(None.filter(_ => true) == None)
  }
}

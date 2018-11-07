package fpinscala

import org.scalatest.FunSuite
import MyModule.{isSorted, fib, curry}

class MyModuleTest extends FunSuite {

  test("-> Exercise 2.1:") {
    assert(fib(10) == 55)
  }

  test("-> Exercise 2.2:") {
    assert(isSorted(Array(2, 4, 6, 8), (x: Int, y: Int) => x <= y))
    assert(!isSorted(Array(2, 4, 8, 6), (x: Int, y: Int) => x <= y))
  }

  test("-> Exercise 2.3: currying works") {
    assert(curry((a:Int, b:Int) => a + b)(2)(3) == 5)
  }
}

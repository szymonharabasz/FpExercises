package fpinscala.errorhandling

import org.scalatest.FunSuite

class EitherTest extends FunSuite {
  test("-> Exercise 4.6 map of Right returns Right of mapped value") {
    assert(Right(2).map(a => a * a) == Right(4))
  }
  test("-> Exercise 4.6 map of Left returns Left of the same value") {
    assert(Left(2).map(_ => "Error") == Left(2))
  }
  test("-> Exercise 4.6 orElse on Right returns it") {
    assert(Right(2).orElse(Right(4)) == Right(2))
  }
  test("-> Exercise 4.6 orElse on Left returns the argument") {
    assert(Left(2).orElse(Right(4)) == Right(4))
  }
  test("-> Exercise 4.6 flatMap of Right returns Right of mapped value") {
    assert(Right(2).flatMap(a => Right(a * a)) == Right(4))
  }
  test("-> Exercise 4.6 flatMap of Left returns Left of the same value") {
    assert(Left(2).flatMap(_ => Left("Error")) == Left(2))
  }
  test("Exercise 4.3 map2 works for two Right arguments") {
    assert(Right(2).map2(Right(4))(_+_) == Right(6))
  }
  test("Exercise 4.3 map2 returns first operand if its Left") {
    assert(Left(2).map2(Right(4))((a:Int, b:Int) => a+b) == Left(2))
  }
  test("Exercise 4.3 map2 returns second operand if its Left") {
    assert(Right(2).map2(Left(4))(_+_) == Left(4))
  }
}

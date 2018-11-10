package fpinscala.errorhandling

import fpinscala.datastrcutures.MyList
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
  test("Exercise 4.5 sequence on MyList[Right[Int],Right[Int],...] returns Right[MyList[Int] " +
    "if there is no Left") {
    assert( Either.sequence(MyList(Right(2), Right(3), Right(4))) == Right(MyList(2,3,4)) )
  }
  test("Exercise 4.5 sequence on MyList[Right[Int],Left[Int],...] returns Left[Int] " +
    "if there is some Left") {
    assert( Either.sequence(MyList(Right(2), Left(3), Right(4))) == Left(3) )
  }

}

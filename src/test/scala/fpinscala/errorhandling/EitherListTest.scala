package fpinscala.errorhandling

import org.scalatest.FunSuite

class EitherListTest extends FunSuite {
  ignore("-> Exercise 4.8 mkPerson with correct arguments returns correct RightList[Person]") {
    assert(Person.mkPerson("Joe", 42) == RightList(Person(new Name("Joe"), new Age(42))))
  }
  ignore("-> Exercise 4.8 mkPerson with wrong name returns one error") {
    assert(Person.mkPerson("", 42) == EitherList("Name is empty."))
  }
  ignore("-> Exercise 4.8 mkPerson with wrong age returns one error") {
    assert(Person.mkPerson("Joe", -42) == EitherList("Age is out of range."))
  }
  test("-> Exercise 4.8 mkPerson with wrong name and age returns two errors") {
    val p = Person.mkPerson("", -42)
    assert(p == EitherList("Name is empty.", "Age is out of range.") ||
      p == EitherList("Age is out of range.", "Name is empty."))
  }

}

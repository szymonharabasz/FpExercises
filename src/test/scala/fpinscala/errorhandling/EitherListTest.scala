package fpinscala.errorhandling

import fpinscala.datastrcutures.MyList
import org.scalatest.FunSuite

class EitherListTest extends FunSuite {
  test("-> Exercise 4.8 mkPerson with correct arguments returns correct RightList[Person]") {
    assert(Person.mkPerson("Joe", 42) == RightList(Person(new Name("Joe"), new Age(42))))
  }
  test("-> Exercise 4.8 mkPerson with wrong name returns one error") {
    assert(Person.mkPerson("", 42) == EitherList("Name is empty."))
  }
  test("-> Exercise 4.8 mkPerson with wrong age returns one error") {
    assert(Person.mkPerson("Joe", -42) == EitherList("Age is out of range."))
  }
  test("-> Exercise 4.8 mkPerson with wrong name and age returns two errors") {
    val p = Person.mkPerson("", -42)
    assert(p == EitherList("Name is empty.", "Age is out of range.") ||
      p == EitherList("Age is out of range.", "Name is empty."))
  }
  test("-> Exercise 4.8 traverse returns a full list of all errors") {
    val func = (a:Int) => if (a % 2 == 0) RightList(a*a) else LeftList(a, NilList)
    assert(EitherList.traverse(MyList(1,2,3,4,5,6))(func) == EitherList(1,3,5))
  }
  test("-> Exercise 4.8 traverse returns a full list of valies if there are no errors") {
    val func = (a:Int) => RightList(a*a)
    assert(EitherList.traverse(MyList(1,2,3,4,5,6))(func) == RightList(MyList(1,4,9,16,25,36)))
  }

}

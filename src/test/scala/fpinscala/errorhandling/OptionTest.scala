package fpinscala.errorhandling

import fpinscala.datastrcutures.MyList
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
  test("-> Exercise 4.1 map selects Option(Stringp) for Option[Employee]") {
    case class Employee(name: String, department: String)
    val joe = Some(Employee("joe", "IT"))
    assert(joe.map(_.department) == Some("IT"))
  }
  test("-> Exercise 4.1 map selects None as department of None Employee") {
    case class Employee(name: String, department: String)
    val joe:Option[Employee] = None
    assert(joe.map(_.department) == None)
  }
  test("-> Exercise 4.2 variance calculates correct value") (
    assert(Option.variance(Seq(2.0,3.0,4.0)) == Some(2.0/3))
  )
  test("-> Exercise 4.3 lift works") {
    val absO: Option[Double] => Option[Double] = Option.lift(math.abs)
    assert(absO(Some(-4.0)) == Some(4.0))
  }
  test("Exercise 4.3 map2 works for two existing arguments") {
    assert(Option.map2(Some(2), Some(4))(_+_) == Some(6))
  }
  test("Exercise 4.3 map2 returns None for one of arguments None") {
    assert(Option.map2(Some(2), None)(_+_) == None)
  }
  test("Exercise 4.4 sequence on MyList[Option[Int]] returns Option[MyList[Int] if there is no None") {
    assert( Option.sequence(MyList(Some(2), Some(3), Some(4))) == Some(MyList(2,3,4)) )
  }
  test("Exercise 4.4 sequence returns None for list containing None") {
    assert( Option.sequence(MyList(Some(2), None, Some(4))) == None )
  }
  test("Exercise 4.5 sequence_trav on MyList[Option[Int]] returns Option[MyList[Int] if there is no None") {
    assert( Option.sequence_trav(MyList(Some(2), Some(3), Some(4))) == Some(MyList(2,3,4)) )
  }
  test("Exercise 4.5 sequence_trav returns None for list containing None") {
    assert( Option.sequence_trav(MyList(Some(2), None, Some(4))) == None )
  }


}

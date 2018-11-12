package fpinscala.errorhandling

case class Person(name: Name, age: Age) {
  // Based on https://alvinalexander.com/scala/
  def canEqual(a: Any) = a.isInstanceOf[Person]
  override def equals(that: Any): Boolean =
    that match {
      case that: Person => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }
  override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + age.hashCode;
    result = prime * result + (if (name == null) 0 else name.hashCode)
    return result
  }
}
sealed class Name(val value: String) {
  def canEqual(a: Any) = a.isInstanceOf[Name]
  override def equals(that: Any): Boolean =
    that match {
      case that: Name => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }
  override def hashCode: Int = {
    return value.hashCode
  }

}
sealed class Age(val value: Int) {
  def canEqual(a: Any) = a.isInstanceOf[Age]
  override def equals(that: Any): Boolean =
    that match {
      case that: Age => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }
  override def hashCode: Int = {
    return value
  }
}

object Person {
  def mkName(name: String): EitherList[String, Name] =
    if (name == "" || name == null) EitherList("Name is empty.")
    else RightList(new Name(name))

  def mkAge(age: Int): EitherList[String, Age] =
    if (age < 0) EitherList("Age is out of range.")
    else RightList(new Age(age))

  def mkPerson(name: String, age: Int): EitherList[String, Person] = {
    mkName(name).map2(mkAge(age))(Person(_,_))
  }
}
package fpinscala.datastrcutures

import org.scalatest.FunSuite

class MyTreeTest extends FunSuite {

  test("-> Exercise 3.25") {
    assert(MyTree.size(Branch(Branch(Leaf(3), Leaf(2)), Leaf(4))) == 3)
  }
  test("-> Exercise 3.26") {
    assert(MyTree.max(Branch(Branch(Leaf(3), Leaf(4)), Leaf(3))) == 4)
  }
  test("-> Exercise 3.27") {
    assert(MyTree.depth(Branch(Branch(Leaf(3), Leaf(4)), Leaf(3))) == 3)
    assert(MyTree.depth(Branch(Leaf(6), Branch(Leaf(5), Branch(Leaf(3), Leaf(4))))) == 4)
  }
}

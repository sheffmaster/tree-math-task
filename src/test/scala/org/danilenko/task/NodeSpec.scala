package org.danilenko.task

import org.danilenko.task.TaskHelper._
import org.danilenko.task.TestHelper._
import org.scalatest.FunSuite

class NodeSpec extends FunSuite {

  val simpleNode = Leaf(1, 1, List(Leaf(2, 2), Leaf(3, 3)))
  val optimumNode = Leaf(1, 10, List(Leaf(2, 20, List(Leaf(2, 20))), Leaf(3, 30)))
  val nonOptimumNode = Leaf(1, 10, List(Leaf(2, 20, List(Leaf(2, 20))), Leaf(3, -10)))

  test("toString on empty node") {
    assertResult("empty")(Empty.toString)
  }

  test("toString on nonEmpty node") {
    assertResult("{10}->List({20}->List({20}->List()), {30}->List())")(optimumNode.toString)
  }

  test("remove empty node should return same node") {
    assertResult(optimumNode)(optimumNode.removeSubTree(Empty))
  }
  test("remove nonEmpty node should return another node") {
    assertResult(Leaf(1, 10, List(Leaf(3, 30))))(optimumNode.removeSubTree(Leaf(2, 20)))
  }

  test("remove root node should return Empty node") {
    assertResult(Empty)(optimumNode.removeSubTree(Leaf(1, 10)))
  }

  test("weight of empty node") {
    assertResult(0)(calcNodeWeight(Empty))
  }

  test("weight of Non empty node") {
    assertResult(6)(calcNodeWeight(simpleNode))
    assertResult(80)(calcNodeWeight(optimumNode))
    assertResult(10)(calcNodeWeight(Leaf(1, 10)))
  }

  test("optimize Empty node must return res(0,0,0)") {
    assertResult(Result(0, 0, 0))(optimizeNodeVer1(Empty))
    assertResult(Result(0, 0, 0))(optimizeNodeVer2(Empty))
    assertResult(Result(0, 0, 0))(optimizeNodeVer3(Empty).result)
  }
  test("optimized node remained the same") {
    assertResult(Result(80, 80, 0))(optimizeNodeVer1(optimumNode))
    assertResult(Result(80, 80, 0))(optimizeNodeVer2(optimumNode))
    assertResult(Result(80, 80, 0))(optimizeNodeVer3(optimumNode).result)
  }

  test("optimized node remained the same 2") {
    assertResult(Result(6, 6, 0))(optimizeNodeVer1(simpleNode))
    assertResult(Result(6, 6, 0))(optimizeNodeVer2(simpleNode))
    assertResult(Result(6, 6, 0))(optimizeNodeVer3(simpleNode).result)
  }

  test("optimize simple node") {
    val node = Leaf(1, 1, List(Leaf(2, 2), Leaf(3, -3)))
    assertResult(Result(0, 3, 1))(optimizeNodeVer1(node))
    assertResult(Result(0, 3, 1))(optimizeNodeVer2(node))
    assertResult(Result(0, 3, 1))(optimizeNodeVer3(node).result)
  }

  test("optimize advanced node") {
    val tree = Leaf(1, 1, List(Leaf(2, -2, List(Leaf(3, 2), Leaf(4, 1))), Leaf(5, -3)))
    assertResult(Result(-1, 2, 1))(optimizeNodeVer1(tree))
    assertResult(Result(-1, 2, 1))(optimizeNodeVer2(tree))
    assertResult(Result(-1, 2, 1))(optimizeNodeVer3(tree).result)
  }

  test("optimize advanced 2 node") {
    val fork = Leaf(1, 1,
      List(Leaf(2, 5,
        List(Leaf(4, -2), Leaf(5, -4,
          List(Leaf(6, 2), Leaf(7, 3))))), Leaf(3, -3,
        List(Leaf(8, 2), Leaf(9, 2)))))
    assertResult(Result(calcNodeWeight(fork), 8, 1))(optimizeNodeVer1(fork))
    assertResult(Result(calcNodeWeight(fork), 8, 1))(optimizeNodeVer2(fork))
    assertResult(Result(calcNodeWeight(fork), 8, 1))(optimizeNodeVer3(fork).result)
  }

  test("optimize advanced 3 node") {
    val fork = Leaf(1, 2,
      List(Leaf(2, -1,
        List(Leaf(3, -1,
          List(Leaf(4, 1,
            List(Leaf(5, 1)))))))))
    assertResult(Result(calcNodeWeight(fork), 2, 0))(optimizeNodeVer1(fork))
    assertResult(Result(calcNodeWeight(fork), 2, 0))(optimizeNodeVer2(fork))
    assertResult(Result(calcNodeWeight(fork), 2, 0))(optimizeNodeVer3(fork).result)
  }

  test("optimize advanced 4 node") {
    val fork = Leaf(1, 2, List(Leaf(2, -1, List(Leaf(3, -1), Leaf(4, 2), Leaf(5, -1, List(Leaf(6, 1), Leaf(7, 1)))))))
    assertResult(Result(calcNodeWeight(fork), 4, 1))(optimizeNodeVer1(fork))
  }

  test("optimize advanced 5 node") {
    val fork = Leaf(1, 100,
      List(Leaf(2, -200,
        List(Leaf(6, 500), Leaf(7, -100))), Leaf(3, -5,
        List(Leaf(4, -1), Leaf(5, 2))), Leaf(8, 10), Leaf(9, -1)))
    println(fork)
    assertResult(Result(calcNodeWeight(fork), 410, 3))(optimizeNodeVer1(fork))
    assertResult(Result(calcNodeWeight(fork), 410, 3))(optimizeNodeVer2(fork))
    assertResult(Result(calcNodeWeight(fork), 410, 3))(optimizeNodeVer3(fork).result)
  }

  test("fake test") {
    val node = createFakeNode(1000)
    assertResult(Result(calcNodeWeight(node), 999, 0))(optimizeNodeVer1(node))
    assertResult(Result(calcNodeWeight(node), 999, 0))(optimizeNodeVer2(node))
    assertResult(Result(calcNodeWeight(node), 999, 0))(optimizeNodeVer3(node).result)

  }
}

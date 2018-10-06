package org.danilenko.task

object TestHelper {
  /**
    * Create node for test's
    *
    * @param nodesCount - amount of created nodes
    * @return - created Node
    */
  def createFakeNode(nodesCount: Int): Node = {
    require(10 <= nodesCount && nodesCount <= 1000)

    def helper(node: Node, i: Int): Node = i match {
      case 10 => node
      case _ => Leaf(nodesCount - 1, nodesCount - 1, List(node))
    }

    helper(Empty, nodesCount)
  }
}

package org.danilenko.task

case class Result(initWeight: Int, fWeight: Int, deleted: Int)

case class NodeWithResult(node: Node, result: Result)

object TaskHelper {

  /**
    * Calculate node weight
    *
    * @param node - original node
    * @return - node weight
    */
  def calcNodeWeight(node: Node): Int = node match {
    case fork: Leaf => fork.children.foldLeft(fork.w)((acc, child) => calcNodeWeight(child) + acc)
    case Empty => 0
  }

  /**
    * Optimize node to take max weight (First version)
    * (This method doesn't use removeSubTree method)
    *
    * @param node -original node
    * @return result of optimization
    */
  def optimizeNodeVer1(node: Node): Result = {

    def optimizeHelper(tree: Node, result: Result): Result = tree match {
      case Empty => result

      case leaf: Leaf if leaf.children.isEmpty => result.copy(fWeight = result.fWeight + leaf.w)

      case leaf: Leaf if leaf.children.nonEmpty =>
        val (remainedChildren, forDel) = leaf.children.map { child =>
          optimizeHelper(child, Result(calcNodeWeight(child), 0, 0))
        }.partition(_.fWeight >= 0)

        val res = remainedChildren.foldLeft(Result(0, 0, forDel.size)) { (ch1: Result, ch2: Result) =>
          Result(ch1.initWeight + ch2.initWeight, ch1.fWeight + ch2.fWeight, ch1.deleted + ch2.deleted)
        }

        if (leaf.w < 0) {
          if (leaf.w > res.fWeight)
            res.copy(initWeight = result.initWeight, fWeight = leaf.w, deleted = res.deleted)
          else
            res.copy(initWeight = res.initWeight + leaf.w, fWeight = res.fWeight + leaf.w)
        } else {
          res.copy(initWeight = result.initWeight, fWeight = res.fWeight + leaf.w)
        }
    }

    optimizeHelper(node, Result(calcNodeWeight(node), 0, 0))
  }

  /**
    * Optimize node to take max weight (Second version)
    * (This method doesn't use removeSubTree method)
    *
    * @param node - original node
    * @return result of optimization
    */
  def optimizeNodeVer2(node: Node): Result = node match {
    case Empty => Result(0, 0, 0)

    case leaf: Leaf if leaf.children.isEmpty => Result(leaf.w, leaf.w, 0)

    case leaf: Leaf if leaf.children.nonEmpty =>
      val optimizedChildren = leaf.children.map(child => (child, optimizeNodeVer2(child)))
      val (pos, neg) = optimizedChildren.partition(res => res._2.initWeight >= 0)

      Result(calcNodeWeight(leaf), leaf.w + pos.map(_._2.fWeight).sum, 0 + pos.map(_._2.deleted).sum + neg.length)
  }

  /**
    * Optimize node to take max weight (Last version)
    * (This method uses removeSubTree method)
    *
    * @param node - original node
    * @return optimized Node with result of optimization
    */
  def optimizeNodeVer3(node: Node): NodeWithResult = node match {
    case Empty => NodeWithResult(Empty, Result(0, 0, 0))

    case leaf: Leaf if leaf.children.isEmpty => NodeWithResult(leaf, Result(leaf.w, leaf.w, 0))

    case leaf: Leaf if leaf.children.nonEmpty =>
      val optimizedChildren = leaf.children.map(optimizeNodeVer3)
      val (remainedChildren, childrenForDelete) = optimizedChildren.partition(_.result.fWeight >= 0)

      val optimizedNode = childrenForDelete.foldLeft[Node] {
        leaf.copy(children = optimizedChildren.map(_.node))
      }((acc, nodeWithResult) => acc.removeSubTree(nodeWithResult.node))

      NodeWithResult(
        node = optimizedNode,
        result = Result(
          initWeight = calcNodeWeight(leaf),
          fWeight = calcNodeWeight(optimizedNode),
          deleted = remainedChildren.map(_.result.deleted).sum + childrenForDelete.size
        )
      )
  }

}

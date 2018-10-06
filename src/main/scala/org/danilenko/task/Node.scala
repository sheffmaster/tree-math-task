package org.danilenko.task

sealed abstract class Node {

  def removeSubTree(n: Node): Node

  def toString: String
}

case object Empty extends Node {
  override def removeSubTree(node: Node): Node = Empty

  override def toString: String = "empty"
}

case class Leaf(id: Int, w: Int, children: List[Node] = List[Node]()) extends Node {
  require(-1000 <= w && w <= 1000)
  require(1 <= id && id <= 1000)

  override def removeSubTree(node: Node): Node = {
    node match {
      case Empty => this
      case deleteNode: Leaf if id == deleteNode.id => Empty
      case _ => Leaf(id, w, children.map(_.removeSubTree(node)).filter(_ != Empty))
    }
  }

  override def toString: String = {
    s"{$w}->" + children.map(child => s"${child.toString}")
  }
}



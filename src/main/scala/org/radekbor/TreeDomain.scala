package org.radekbor

sealed abstract class Tree[+T] {
  def isMirrorOf[U](tree: Tree[U]): Boolean

  def isSymetric: Boolean

  def addNode[U >: T](x: U)(implicit ordered: T => Ordered[U]): Tree[U]

}

case class Node[T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = {
    "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  }

  def isSymetric: Boolean = {
    this.isMirrorOf(this)
  }

  def isMirrorOf[U](tree: Tree[U]): Boolean = tree match {
    case t: Node[U] => left.isMirrorOf(t.right) && right.isMirrorOf(t.left) && t.value == value
    case _ => false
  }

  override def addNode[U >: T](x: U)(implicit ordered: T => Ordered[U]): Tree[U] = {
    if (ordered.apply(value) > x) {
      Node(value, left.addNode(x), right)
    } else {
      Node(value, left, right.addNode(x))
    }
  }
}

case object End extends Tree[Nothing] {
  override def toString = "."

  override def isMirrorOf[U](tree: Tree[U]): Boolean = {
    tree match {
      case End => true
      case _ => false
    }
  }

  override def isSymetric: Boolean = true

  override def addNode[U >: Nothing](x: U)(implicit ordered: Nothing => Ordered[U]): Tree[U] = {
    Node(x)
  }
}

object Node {
  def apply[T](value: T): Node[T] = new Node(value, End, End)
  def apply[T](value: T, node: Node[T]): Node[T] = new Node(value, node, End)
}

object Tree {
  def fromList[T](args: T*)(implicit ordered: T => Ordered[T]): Tree[T] = {
    args.foldLeft(End: Tree[T])((tree, x) => tree.addNode(x))
  }
}
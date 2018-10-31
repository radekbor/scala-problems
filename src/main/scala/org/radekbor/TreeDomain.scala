package org.radekbor

sealed abstract class Tree[+T] {
  def isMirrorOf[V](tree: Tree[V]): Boolean

  def isSymetric: Boolean
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  def isSymetric: Boolean = {
    this.isMirrorOf(this)
  }

  def isMirrorOf[V](tree: Tree[V]): Boolean = tree match {
    case t: Node[V] => left.isMirrorOf(t.right) && right.isMirrorOf(t.left) && t.value == value
    case _ => false
  }

}

case object End extends Tree[Nothing] {
  override def toString = "."

  override def isMirrorOf[V](tree: Tree[V]): Boolean = {
    tree match {
      case End => true
      case _ => false
    }
  }

  override def isSymetric: Boolean = true
}

object Node {
  def apply[T](value: T): Node[T] = new Node(value, End, End)
}
package org.radekbor

import scala.collection.immutable
import scala.util.control.TailCalls.TailRec


class CodesSolver {

  def gray(len: Int): List[String] = {
    def append(str: String, strings: List[String]): List[String] = {
      strings.map(code => str + code)
    }

    if (len == 1) {
      List("0", "1")
    } else {
      val firstHalf = gray(len - 1)
      val secondHalf = firstHalf.reverse

      append("0", firstHalf) ::: append("1", secondHalf)
    }
  }

  def huffman(in: List[(String, Int)]): List[(String, String)] = {

    def mergeTwo(tree1: InnerTree[String, Int], tree2: InnerTree[String, Int]): InnerTree[String, Int] = {
      InnerTree("", tree1.t2 + tree2.t2, tree1, tree2)
    }

    def merge(trees: List[InnerTree[String, Int]]): List[InnerTree[String, Int]] = {
      if (trees.length > 1) {
        mergeTwo(trees.head, trees.tail.head) :: trees.tail.tail
      } else {
        trees
      }
    }

    def encode(tree: InnerTree[String, Int], prefix: String = ""): List[(String, String)] = {
      var result = List.empty[(String, String)]
      if (tree.left != null) {
        result = encode(tree.left, prefix + "0") ::: result
      }
      if (tree.right != null) {
        result = encode(tree.right, prefix + "1") ::: result
      }
      if (tree.t1.nonEmpty) {
        List((tree.t1, prefix))
      } else {
        result
      }
    }

    def buildTree(trees: List[InnerTree[String, Int]]): List[InnerTree[String, Int]] = {
      if (trees.length == 1) {
        trees
      } else {
        val merged = merge(trees).sortBy(_.t2)
        buildTree(merged)
      }
    }

    val trees = in.map(pair => InnerTree(pair._1, pair._2)).sortBy(_.t2)
    val tree = buildTree(trees).head
    encode(tree).sortBy(_._1)
  }
}

case class InnerTree[T1, T2](t1: T1, t2: T2, left: InnerTree[T1, T2] = null, right: InnerTree[T1, T2] = null)
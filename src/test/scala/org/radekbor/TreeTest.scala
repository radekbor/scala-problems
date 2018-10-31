package org.radekbor

import org.scalatest.{FunSuite, Matchers}

class TreeTest extends FunSuite with Matchers {

  test("56a) Should return true when isSymetric for single Node") {
    Node(1).isSymetric should be (true)
  }

  test("56b) Should return true when isSymetric for Tree 1") {
    Node(1, Node(2), Node(2)).isSymetric should be (true)
  }

  test("56d) Should return true when isSymetric for Tree 2") {
    val tree = Node(1,
      Node(2, End, Node(3)),
      Node(2, Node(3), End)
    )

    tree.isSymetric should be (true)
  }

  test("56d) Should return false when isSymetric for Tree 3") {
    val tree = Node(1,
      Node(2, End, Node(3)),
      Node(2, End, Node(3))
    )

    tree.isSymetric should be (false)
  }

  test("56e) Should return false when isSymetric for Tree 4") {
    val tree = Node(1,
      Node(2, Node(3), Node(4)),
      Node(2, Node(3), Node(4))
    )

    tree.isSymetric should be (false)
  }


}

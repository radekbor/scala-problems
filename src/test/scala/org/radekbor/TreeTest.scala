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

  test("57a) Add node test") {
    val tree = Node(2).addNode(1).addNode(3)

    tree should be (Node(2, Node(1), Node(3)))

  }

  test("57b) Add node test") {
    val tree = Node(3).addNode(1).addNode(2)

    tree should be (Node(3, Node(1, End, Node(2))))

  }

  test("57) From List") {
    val tree = Tree.fromList(3, 1, 2, 4)

    val expected = Node(3, Node(1, End, Node(2)), Node(4))

    tree should be (expected)

  }

  test("61a) Count leaves") {
    val tree = Tree.fromList(3)

    tree.leaves should be (1)
  }

  test("61b) Count leaves") {
    val tree = Tree.fromList(3, 2)

    tree.leaves should be (1)
  }

  test("61c) Count leaves") {
    val tree = Tree.fromList(3, 1, 2, 4)

    tree.leaves should be (2)
  }

  test("61*a) Get leaves") {
    val tree = Tree.fromList(3, 2)

    tree.allLeaves should be (List(2))
  }

  test("61*b) Get leaves") {
    val tree = Tree.fromList(3, 1, 2, 4)

    tree.allLeaves should be (List(2, 4))
  }

  test("62a) Internal Nodes") {
    val tree = Tree.fromList(3)

    tree.internalNodes should be (Nil)
  }

  test("62b) Internal Nodes") {
    val tree = Tree.fromList(3, 1, 2, 4)
    println(tree)
    tree.internalNodes should be (List(3, 1))
  }


}

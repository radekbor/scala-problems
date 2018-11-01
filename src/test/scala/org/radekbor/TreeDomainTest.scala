package org.radekbor

import org.scalatest.FunSuite

class TreeDomainTest extends FunSuite {

  test("Create Tree") {
    val tree = Node(1,
      Node(2),
      Node(3)
    )
    println(tree)
  }
}

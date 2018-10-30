package org.radekbor

import org.scalatest.{FunSuite, Matchers}

class CodesSolverTest extends FunSuite with Matchers {

  val solver = new CodesSolver()

  test("49a) Should return codes for length 1") {
    solver.gray(1) should be(List("0", "1"))
  }

  test("49b) Should return codes for length 2") {
    solver.gray(2) should be(List("00", "01", "11", "10"))
  }

  test("49c) Should return codes for length 3") {
    solver.gray(3) should be(List("000", "001", "011", "010", "110", "111", "101", "100"))
  }

  test("50) Huffman codes") {
    val result: List[(String, String)] = solver.huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
    result should be (List(("a", "0"), ("b", "101"), ("c", "100"), ("d", "111"), ("e", "1101"), ("f", "1100")))

  }
}

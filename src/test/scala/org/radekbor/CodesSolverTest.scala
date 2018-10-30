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

}

package org.radekbor

import org.scalatest.{FunSuite, Matchers}
import org.radekbor.LogicSolver._

class LogicSolverTest extends FunSuite with Matchers {

  val xorTestData = List(
    ((true, true), false),
    ((true, false), true),
    ((false, true), true),
    ((false, false), false)
  )

  for ((in, out) <- xorTestData) {
    test(s"46a)  Should return ${out} value for xor with args ${in}") {

      (LogicSolver.xor _).tupled(in) should be (out)
    }
  }

  for ((in, out) <- xorTestData) {
    test(s"47a)  Should return ${out} value for xor operator for args ${in}") {

      val result = in._1 xor in._2

      result should be (out)
    }
  }

}


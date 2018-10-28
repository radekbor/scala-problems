package org.radekbor

import org.radekbor.ArithmeticTaskSolver._
import org.scalatest.{FunSuite, Matchers}


class LazyPrimeImprovedTest extends FunSuite with Matchers {

  test(s"Prime stream") {
    val stream = new ArithmeticTaskSolver().primes
    for (i <- 0 to 10) {
      val prime = stream(i)
      println(s"$i: ${prime}")
    }
  }

}
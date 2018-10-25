package org.radekbor

import org.scalatest.{FunSuite, Matchers}
import org.radekbor.ArithmeticTaskSolver._


class ArithmeticTaskSolverTest extends FunSuite with Matchers {

  val primes = List(1, 2, 3, 5, 7, 11, 13, 17)
  val notPrimes = (1 to 17).toSet -- Set(1, 2, 3, 5, 7, 11, 13, 17)

  for (num <- primes) {
    test(s"31) Should return true when ${num}.isPrime") {
      num.isPrime should be(true)
    }
  }

  for (num <- notPrimes) {
    test(s"31) Should return false when ${num}.isPrime") {
      num.isPrime should be(false)
    }
  }

}
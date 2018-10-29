package org.radekbor

import org.scalatest.{FunSuite, Matchers}
import org.radekbor.ArithmeticTaskSolver._


class ArithmeticTaskSolverTest extends FunSuite with Matchers {

  val primes = List(1, 2, 3, 5, 7, 11, 13, 17)
  val notPrimes = (1 to 17).toSet -- Set(1, 2, 3, 5, 7, 11, 13, 17)

  val solver = new ArithmeticTaskSolver()

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

  val gcdData = List(
    (2, 4, 2),
    (5, 35, 15),
    (1, 7, 11),
    (1, 1, 3)
  )

  for (data <- gcdData) {
    test(s"32) Should return ${data._1} when gcd(${data._2}, ${data._3}") {
      solver.gcd(data._2, data._3) should be(data._1)
    }
  }

  for (data <- gcdData) {
    test(s"33) Should return ${data._1 == 1} when ${data._2} isCoprime to ${data._3}") {
      solver.areCoprime(data._2, data._3) should be(data._1 == 1)
    }
  }

  val totientData = List(
    (4, 10),
    (2, 3),
    (6, 7),
    (10, 11),
    (12, 13),
    (8, 20),
  )

  for (data <- totientData) {
    test(s"34) Should return ${data._1} totientOf ${data._2}") {
      solver.totientOf(data._2) should be(data._1)
    }
  }

  val primeFactors = List(
    (List(2, 3), 6),
    (List(2, 2, 2), 8),
    (List(2, 2, 2, 2), 16)
  )

  for (data <- primeFactors) {
    test(s"35) Should return ${data._1} for prime factors of${data._2}") {
      solver.primeFactors(data._2) should be(data._1)
    }
  }

  val primeFactorMultiplicity = List(
    (List((2, 1), (3, 1)), 6),
    (List((2, 3)), 8),
    (List((2, 4)), 16)
  )

  for (data <- primeFactorMultiplicity) {
    test(s"36) Should return ${data._1} for prime factors of${data._2}") {
      solver.primeFactorsMultiplicity(data._2) should be(data._1)
    }
  }

  for (data <- totientData) {
    test(s"37) Should return ${data._1} eulerThi ${data._2}") {
      solver.eulerThi(data._2) should be(data._1)
    }
  }

  test(s"38) Should return equal value for eulerThi and totient") {
    solver.eulerThi(10090) should be(solver.totientOf(10090))
  }

  test(s"39) Get all primes from range") {
    val result = solver.primes.filter(_ >= 7).takeWhile(_ <= 31)
    result should be(List(7, 11, 13, 17, 19, 23, 29, 31))
  }

  val goldbachData = List(
    (4, (2, 2)),
    (6, (3, 3)),
    (8, (3, 5)),
    (10, (3, 7)),
    (12, (5, 7))
  )

  for (data <- goldbachData) {
    test(s"40) Should return ${data._2} for ${data._1}.goldbach") {
      val result = data._1.Goldbach
      result should be(data._2)
    }
  }

  test("41) Should return goldbach list for range") {
    val solver = new ArithmeticTaskSolver()

    val result = solver.goldbachRange(9, 20)

    result should be (List((3, 7), (5, 7), (3, 11), (3, 13), (5, 13), (3, 17)))

  }

  test("42) Should return goldbach with min prime value") {
    val solver = new ArithmeticTaskSolver()

    val result: List[(Int, Int)] = solver.goldbachRangeLimited(3, 2000, 50)

    result should be (List((73, 919), (61, 1321), (67, 1789), (61, 1867)))

  }
}
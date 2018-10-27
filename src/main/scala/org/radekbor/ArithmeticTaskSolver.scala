package org.radekbor

import org.radekbor.ArithmeticTaskSolver.ExtendedInt

import scala.collection.mutable

object ArithmeticTaskSolver {


  implicit class ExtendedInt(value: Int) {

    def isPrime: Boolean = {
      if (value == 1 || value == 2) {
        true
      } else {
        val max: Int = (value / 2) + 1
        !Stream.range(2, max)
          .exists(x => value % x == 0)
      }
    }
  }

}

class ArithmeticTaskSolver {

  def gcd(a: Int, b: Int): Int = {
    if (a == b) {
      a
    } else {
      if (a > b) {
        gcd(a - b, b)
      } else {
        gcd(b - a, a)
      }
    }
  }

  def areCoprime(a: Int, b: Int): Boolean = {
    gcd(a, b) == 1
  }

  def totientOf(a: Int): Int = {
    Range.inclusive(1, a)
      .count(x => {
        areCoprime(x, a)
      })
  }

  def primeFactors(a: Int): List[Int] = {
    def primeFactorsInternal(a: Int, l: Seq[Int]): List[Int] = {
      if (a.isPrime) {
        List(a)
      } else {
        if (a % l.head == 0) {
          l.head :: primeFactorsInternal(a / l.head, l)
        } else {
          primeFactorsInternal(a, l.tail)
        }
      }
    }

    val data = for (
      i <- Range(2, a) if new ExtendedInt(i).isPrime
    ) yield i
    primeFactorsInternal(a, data)
  }

  def primeFactorsMultiplicity(a: Int): List[(Int, Int)] = {
    def join(in: List[Int]): List[(Int, Int)] = {
      if (in.isEmpty) {
        List.empty[(Int, Int)]
      } else {
        val groups = in.span(_ == in.head)
        (in.head, groups._1.length) :: join(groups._2)
      }
    }

    val ints = primeFactors(a)
    join(ints)
  }

}
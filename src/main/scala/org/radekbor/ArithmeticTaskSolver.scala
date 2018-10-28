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

    def Goldbach: (Int, Int) = {
      val solver = new ArithmeticTaskSolver()
      val primes = solver.primes.takeWhile(_ < value).toList

      def inner(primes1: List[Int], primes2: List[Int]): (Int, Int) = {
        val sum = primes1.head + primes2.head
        sum match {
          case c if c == value => (primes1.head, primes2.head)
          case c if c < value => inner(primes1.tail, primes2)
          case c if c > value => inner(primes1, primes2.tail)
        }
      }

      val result = inner(primes, primes.reverse)
      if (result._1 > result._2) {
        (result._2, result._1)
      } else {
        result
      }
    }
  }

}

class ArithmeticTaskSolver {

  lazy val primes: Stream[Int] = {
    def sieve(s: Stream[Int]): Stream[Int] = {
      s.head #:: sieve(s.tail filter (_ % s.head != 0))
    }

    sieve(Stream.from(2))
  }

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

  def eulerThi(a: Int): Double = {
    val factors = primeFactorsMultiplicity(a)
    factors.map(a => {
      (a._1 - 1) * Math.pow(a._1, a._2 - 1)
    })
      .foldLeft(1.0)(_ * _)
  }

}
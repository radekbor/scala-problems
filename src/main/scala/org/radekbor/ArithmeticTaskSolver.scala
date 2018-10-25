package org.radekbor

object ArithmeticTaskSolver {


  implicit class ExtendedInt(value: Int) {

    def isPrime: Boolean = {
      if (value == 1 || value == 2) {
        true
      } else {
        val max: Int = (value / 2) + 1
        println(s"${value} ${max}")
        !Stream.range(2, max)
          .exists(x => value % x == 0)
      }
    }
  }

}


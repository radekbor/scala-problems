package org.radekbor

object LogicSolver {

  implicit class Logic(left: Boolean) {

    def xor(right: Boolean): Boolean = {
      LogicSolver.xor(left, right)
    }
  }

  def xor(a: Boolean, b: Boolean): Boolean = {
    a ^ b
  }

  def or(a: Boolean, b: Boolean): Boolean = {
    a | b
  }

  def and(a: Boolean, b: Boolean): Boolean = {
    a & b
  }

}

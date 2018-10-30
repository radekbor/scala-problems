package org.radekbor

class CodesSolver {

  def gray(len: Int): List[String] = {
    if (len == 1) {
      List("0", "1")
    } else {
      val firstHalf = gray(len - 1)
      val secondHalf = firstHalf.reverse

      append("0", firstHalf) ::: append("1", secondHalf)
    }
  }

  private def append(str: String, strings: List[String]) : List[String] = {
    strings.map(code => str + code)
  }
}

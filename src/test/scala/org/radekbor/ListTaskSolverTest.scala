package org.radekbor

import org.scalatest.FunSuite
import org.scalatest.matchers
import org.scalatest._

class ListTaskSolverTest extends FunSuite with Matchers {

  test("find last") {
    val in = List(1, 2, 3)
    val solver = new ListTaskSolver()

    val result = solver.findLast(in)

    result should be(3)
  }
  test("find penultimate") {
    val in = List(1, 2, 3, 4, 5)
    val solver = new ListTaskSolver()

    val result = solver.findPenultimate(in)

    result should be(4)
  }

  test("1) find nth") {
    val in = List(1, 2, 3, 4, 5)
    val solver = new ListTaskSolver()

    val result = solver.nth(in, 2)

    result should be(3)
  }

  test("2) get length when 5 elements") {
    val in = List(1, 2, 3, 4, 5)
    val solver = new ListTaskSolver()

    val result = solver.length(in)

    result should be(5)
  }

  test("3) get length when 1 element") {
    val in = List(1)
    val solver = new ListTaskSolver()

    val result = solver.length(in)

    result should be(1)
  }

  test("4) should reverse") {
    val in = List(5, 4, 3, 2, 1)
    val solver = new ListTaskSolver()

    val result = solver.revers(in)

    result should be(List(1, 2, 3, 4, 5))
  }

  test("5) should return true when palidrome") {
    val in = List(5, 4, 3, 4, 5)
    val solver = new ListTaskSolver()

    val result = solver.isPalindrome(in)

    result should be(true)
  }

  test("6) should return false when not palidrome") {
    val in = List(5, 4, 3, 4, 1)
    val solver = new ListTaskSolver()

    val result = solver.isPalindrome(in)

    result should be(false)
  }

  test("7) should flatten map") {
    val in = List(List(1, 2), List(3, 4))
    val solver = new ListTaskSolver()

    val result = solver.flatten(in)

    result should be(List(1, 2, 3, 4))
  }

  test("8) should compress list") {
    val in = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e')
    val solver = new ListTaskSolver()

    val result = solver.compress(in)

    result should be(List('a, 'b, 'c, 'a, 'd, 'e))
  }

  test("9) should pack list into sublist") {
    val in = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val solver = new ListTaskSolver()

    val result = solver.pack(in)

    result should be(List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  test("10) should encode") {

    val in = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val solver = new ListTaskSolver()

    val result = solver.encode(in)

    result should contain theSameElementsAs List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
  }

  test("11) should encode2") {

    val in = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val solver = new ListTaskSolver()

    val result = solver.encode2(in)

    result should contain theSameElementsAs List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e))
  }

  test("12) should decode") {

    val in = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    val solver = new ListTaskSolver()

    val result = solver.decode(in)

    result should contain theSameElementsAs List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  }

  test("13) should decode") {

    val in = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val solver = new ListTaskSolver()

    val result = solver.encodeDirect(in)

    result should be (List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  test("14) should decode") {

    val in = List('a, 'b, 'c, 'c, 'd)
    val solver = new ListTaskSolver()

    val result = solver.duplicate(in)

    result should be (List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

    test("15) should decode") {

      val in =  List('a, 'b, 'c, 'c, 'd)
      val solver = new ListTaskSolver()

      val result = solver.duplicate(3, in)

      result should be (List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
    }

    test("17) drop nth") {

      val in =  List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      val solver = new ListTaskSolver()

      val result = solver.drop(3, in)

      result should be (List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
    }

    test("18) Slice") {

      val in =  List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      val solver = new ListTaskSolver()

      val result = solver.slice(3, 7, in)

      result should be (List('d, 'e, 'f, 'g))
    }

    test("19) Rotate") {
      val in = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      val solver = new ListTaskSolver()

      val result = solver.rotate(3, in)

      result should be (List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    }

    test("19) Rotate reg") {
      val in = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      val solver = new ListTaskSolver()

      val result = solver.rotate2(3, in)

      result should be (List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    }

    test("20) Remove nth") {
      val in = List('a, 'b, 'c, 'd)
      val solver = new ListTaskSolver()

      val result = solver.removeAt(1, in)

      result should be (List('a, 'c, 'd),'b)
    }

}

package org.radekbor

import org.scalatest.FunSuite
import org.scalatest.matchers
import org.scalatest._

class ListTaskSolverTest extends FunSuite with Matchers {

  test("find last") {
    val in = List(1, 2, 3)
    val solver = new ListTaskSolver()

    val result = solver.findLast(in)

    result should be (3)
  }
  test("find penultimate") {
    val in = List(1, 2, 3, 4, 5)
    val solver = new ListTaskSolver()

    val result = solver.findPenultimate(in)

    result should be (4)
  }

  test("find nth") {
    val in = List(1, 2, 3, 4, 5)
    val solver = new ListTaskSolver()

    val result = solver.nth(in, 2)

    result should be (3)
  }

  test("get length when 5 elements") {
    val in = List(1, 2, 3, 4, 5)
    val solver = new ListTaskSolver()

    val result = solver.length(in)

    result should be (5)
  }

  test("get length when 1 element") {
    val in = List(1)
    val solver = new ListTaskSolver()

    val result = solver.length(in)

    result should be (1)
  }

  test("should reverse") {
    val in = List(5, 4, 3, 2, 1)
    val solver = new ListTaskSolver()

    val result = solver.revers(in)

    result should be (List(1, 2, 3, 4, 5))
  }

  test("should return true when palidrome") {
    val in = List(5, 4, 3, 4, 5)
    val solver = new ListTaskSolver()

    val result = solver.isPalindrome(in)

    result should be (true)
  }

  test("should return false when not palidrome") {
    val in = List(5, 4, 3, 4, 1)
    val solver = new ListTaskSolver()

    val result = solver.isPalindrome(in)

    result should be (false)
  }

  test("should flatten map") {
    val in = List(List(1, 2), List(3, 4))
    val solver = new ListTaskSolver()

    val result = solver.flatten(in)

    result should be (List(1, 2, 3, 4))
  }

  test("should compress list") {
    val in = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e')
    val solver = new ListTaskSolver()

    val result = solver.compress(in)

    result should be (List('a, 'b, 'c, 'a, 'd, 'e))
  }

  test("should pack list into sublist") {
    val in = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val solver = new ListTaskSolver()

    val result = solver.pack(in)

    result should be (List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  test("should encode") {

    val in = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val solver = new ListTaskSolver()

    val result = solver.encode(in)

    result should contain theSameElementsAs List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  }




}

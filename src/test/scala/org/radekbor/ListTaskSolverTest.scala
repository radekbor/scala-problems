package org.radekbor

import org.scalatest.{FunSuite, _}

class ListTaskSolverTest extends FunSuite with Matchers {

  val solver = new ListTaskSolver()

  test("find last") {
    val in = List(1, 2, 3)

    val result = solver.findLast(in)

    result should be(3)
  }

  test("find penultimate") {
    val in = List(1, 2, 3, 4, 5)

    val result = solver.findPenultimate(in)

    result should be(4)
  }

  test("1) find nth") {
    val in = List(1, 2, 3, 4, 5)

    val result = solver.nth(in, 2)

    result should be(3)
  }

  test("2) get length when 5 elements") {
    val in = List(1, 2, 3, 4, 5)

    val result = solver.length(in)

    result should be(5)
  }

  test("3) get length when 1 element") {
    val in = List(1)

    val result = solver.length(in)

    result should be(1)
  }

  test("4) should reverse") {
    val in = List(5, 4, 3, 2, 1)

    val result = solver.revers(in)

    result should be(List(1, 2, 3, 4, 5))
  }

  test("5) should return true when palidrome") {
    val in = List(5, 4, 3, 4, 5)

    val result = solver.isPalindrome(in)

    result should be(true)
  }

  test("6) should return false when not palidrome") {
    val in = List(5, 4, 3, 4, 1)

    val result = solver.isPalindrome(in)

    result should be(false)
  }

  test("7) should flatten map") {
    val in = List(List(1, 2), List(3, 4))


    val result = solver.flatten(in)

    result should be(List(1, 2, 3, 4))
  }

  test("8) should compress list") {
    val in = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e')


    val result = solver.compress(in)

    result should be(List('a, 'b, 'c, 'a, 'd, 'e))
  }

  test("9) should pack list into sublist") {
    val in = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)


    val result = solver.pack(in)

    result should be(List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  test("10) should encode") {

    val in = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)


    val result = solver.encode(in)

    result should contain theSameElementsAs List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
  }

  test("11) should encode2") {

    val in = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)


    val result = solver.encode2(in)

    result should contain theSameElementsAs List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e))
  }

  test("12) should decode") {

    val in = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))


    val result = solver.decode(in)

    result should contain theSameElementsAs List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  }

  test("13) should decode") {

    val in = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)


    val result = solver.encodeDirect(in)

    result should be(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  test("14) should decode") {

    val in = List('a, 'b, 'c, 'c, 'd)


    val result = solver.duplicate(in)

    result should be(List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  test("15) should decode") {

    val in = List('a, 'b, 'c, 'c, 'd)


    val result = solver.duplicate(3, in)

    result should be(List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  test("17) drop nth") {

    val in = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)


    val result = solver.drop(3, in)

    result should be(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  test("18) Slice") {

    val in = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)


    val result = solver.slice(3, 7, in)

    result should be(List('d, 'e, 'f, 'g))
  }

  test("19) Rotate") {
    val in = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)


    val result = solver.rotate(3, in)

    result should be(List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
  }

  test("19) Rotate reg") {
    val in = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)


    val result = solver.rotate2(3, in)

    result should be(List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
  }

  test("20) Remove nth") {
    val in = List('a, 'b, 'c, 'd)


    val result = solver.removeAt(1, in)

    result should be(List('a, 'c, 'd), 'b)
  }

  test("21) Add at nth") {
    val in = List('a, 'b, 'c, 'd)


    val result = solver.addAt('new, 1, in)

    result should be(List('a, 'new, 'b, 'c, 'd))
  }

  test("22) Add at nth") {


    val result = solver.range(4, 9)

    result should be(List(4, 5, 6, 7, 8, 9))

  }
  /*
  test("23) Random select") {
    

    val result = solver.randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))


    // TODO
  }

  test("24) Get N random") {
    

    val result = solver.getNRadom(3, 45)

    // TODO
  }

  test("25) random permutation") {

    val in = List('a, 'b, 'c, 'd, 'e, 'f)

    

    val result1 = solver.randomPermute(in)
    val result2 = solver.randomPermute(in)

    result1 should contain theSameElementsAs in
    result2 should contain theSameElementsAs in
    result1 should not be result2
    // TODO
  }

test("26) Generate permuations") {

  

  val result = solver.combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))

  // TODO
}

test("27) Group the elements of a set into disjoint subsets.") {


  // TODO
}
*/

  test("28) Sorting a list of lists according to length of sublists.") {


    val in = (List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))

    val result = solver.sortByLengthOfSublist(in)

    result should be(List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l)))
  }

  test("29) Sorting a list of lists according to frequency.") {

    val in = List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))

    val result = solver.lsortFreq(in)
    // TODO
    //    result should be (List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l)))
  }


}
